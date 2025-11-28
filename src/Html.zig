const std = @import("std");
const Allocator = std.mem.Allocator;

/// ## The Abstract Syntax Tree (AST)
///
/// Defines the structure of the parsed HTML.
pub const NodeType = enum {
    Root,
    Element,
    Text,
    Comment,
    Doctype,
};

pub const Attribute = struct {
    name: []const u8,
    value: ?[]const u8,
};

/// A single node in the AST.
/// Uses the same first-child/next-sibling structure as Vaporize.zig.
pub const Node = struct {
    tag: NodeType,

    data: union {
        element: struct {
            tag_name: []const u8,
            attributes: []const Attribute,
            is_self_closing: bool,
        },
        text: struct {
            content: []const u8,
        },
        comment: struct {
            content: []const u8,
        },
        doctype: struct {
            content: []const u8,
        },
        none: void,
    },

    child: ?*Node, // Pointer to the first child node
    next: ?*Node, // Pointer to the next sibling node
};

/// ## The Parser
pub const Parser = struct {
    arena: Allocator,
    source: []const u8,
    pos: usize,

    pub fn init(allocator: Allocator, source: []const u8) Parser {
        return Parser{
            .arena = allocator,
            .source = source,
            .pos = 0,
        };
    }

    /// Helper to create a new Node using the arena.
    fn createNode(self: *Parser, tag: NodeType) !*Node {
        const node = try self.arena.create(Node);
        node.* = .{
            .tag = tag,
            .data = .{ .none = {} },
            .child = null,
            .next = null,
        };
        return node;
    }

    /// Entry point for parsing
    pub fn parse(self: *Parser) !*Node {
        const root = try self.createNode(.Root);
        // Parse content with no specific closing tag requirement (expects EOF)
        try self.parseNodes(root, null);
        return root;
    }

    /// Recursively parses nodes and attaches them to `parent`
    /// Stops when it hits EOF or the `closing_tag` if provided.
    fn parseNodes(self: *Parser, parent: *Node, closing_tag: ?[]const u8) anyerror!void {
        var last_child: ?*Node = null;

        while (!self.isAtEnd()) {
            // Check for closing tag of the current parent
            if (closing_tag) |tag| {
                if (self.matchClosingTag(tag)) {
                    // Consume the closing tag and return (popping stack)
                    self.consumeClosingTag();
                    return;
                }
            }

            const char = self.peek();
            var node: *Node = undefined;

            if (char == '<') {
                if (self.peekNext() == '!') {
                    if (self.match("<!--")) {
                        node = try self.parseComment();
                    } else if (self.match("<!DOCTYPE") or self.match("<!doctype")) {
                        node = try self.parseDoctype();
                    } else {
                        // Unknown, skip
                        _ = self.advance();
                        continue;
                    }
                } else if (self.peekNext() == '/') {
                    // Closing tag without matching open? Skip or error.
                    break;
                } else {
                    node = try self.parseElement();
                }
            } else {
                node = try self.parseText();
            }

            // Attach node as a child of parent
            if (last_child == null) {
                parent.child = node;
            } else {
                last_child.?.next = node;
            }
            last_child = node;
        }
    }

    fn parseElement(self: *Parser) !*Node {
        _ = self.advance(); // consume '<'
        self.skipWhitespace();

        // Parse tag name
        const tag_start = self.pos;
        while (!self.isAtEnd() and isAlphanumeric(self.peek())) {
            _ = self.advance();
        }
        const tag_name = self.source[tag_start..self.pos];

        // Parse attributes
        var attrs = std.array_list.Managed(Attribute).init(self.arena);
        while (!self.isAtEnd()) {
            self.skipWhitespace();
            const c = self.peek();
            if (c == '>' or c == '/') break;

            // Attribute name
            const attr_start = self.pos;
            while (!self.isAtEnd() and isAlphanumeric(self.peek())) {
                _ = self.advance();
            }
            const attr_name = self.source[attr_start..self.pos];

            self.skipWhitespace();

            var attr_value: ?[]const u8 = null;
            if (self.peek() == '=') {
                _ = self.advance();
                self.skipWhitespace();
                const quote = self.peek();
                if (quote == '"' or quote == '\'') {
                    _ = self.advance();
                    const val_start = self.pos;
                    while (!self.isAtEnd() and self.peek() != quote) {
                        _ = self.advance();
                    }
                    attr_value = self.source[val_start..self.pos];
                    if (self.peek() == quote) _ = self.advance();
                }
            }

            try attrs.append(.{ .name = attr_name, .value = attr_value });
        }

        self.skipWhitespace();
        const is_self_closing = self.peek() == '/';
        if (is_self_closing) _ = self.advance();

        if (self.peek() == '>') _ = self.advance();

        const node = try self.createNode(.Element);
        node.data = .{
            .element = .{
                .tag_name = tag_name,
                .attributes = try attrs.toOwnedSlice(),
                .is_self_closing = is_self_closing,
            },
        };

        // If not self-closing and not void, parse children
        if (!is_self_closing and !isVoidElement(tag_name)) {
            try self.parseNodes(node, tag_name);
        }

        return node;
    }

    fn parseText(self: *Parser) !*Node {
        const start = self.pos;
        while (!self.isAtEnd() and self.peek() != '<') {
            _ = self.advance();
        }
        const content = self.source[start..self.pos];

        const node = try self.createNode(.Text);
        node.data = .{ .text = .{ .content = content } };
        return node;
    }

    fn parseComment(self: *Parser) !*Node {
        // Consume "<!--"
        _ = self.advance();
        _ = self.advance();
        _ = self.advance();
        _ = self.advance();

        const start = self.pos;
        while (!self.isAtEnd()) {
            if (self.match("-->")) break;
            _ = self.advance();
        }
        const content = self.source[start..self.pos];

        // Consume "-->"
        if (!self.isAtEnd()) {
            _ = self.advance();
            _ = self.advance();
            _ = self.advance();
        }

        const node = try self.createNode(.Comment);
        node.data = .{ .comment = .{ .content = content } };
        return node;
    }

    fn parseDoctype(self: *Parser) !*Node {
        const start = self.pos;
        while (!self.isAtEnd() and self.peek() != '>') {
            _ = self.advance();
        }
        const content = self.source[start..self.pos];
        if (self.peek() == '>') _ = self.advance();

        const node = try self.createNode(.Doctype);
        node.data = .{ .doctype = .{ .content = content } };
        return node;
    }

    /// Utilities
    fn matchClosingTag(self: *Parser, tag: []const u8) bool {
        if (self.pos + 2 + tag.len > self.source.len) return false;
        if (self.source[self.pos] != '<') return false;
        if (self.source[self.pos + 1] != '/') return false;

        const potential_tag = self.source[self.pos + 2 .. self.pos + 2 + tag.len];
        return std.mem.eql(u8, potential_tag, tag);
    }

    fn consumeClosingTag(self: *Parser) void {
        while (!self.isAtEnd() and self.peek() != '>') {
            _ = self.advance();
        }
        if (self.peek() == '>') _ = self.advance();
    }

    fn match(self: *Parser, expected: []const u8) bool {
        if (self.pos + expected.len > self.source.len) return false;
        return std.mem.eql(u8, self.source[self.pos .. self.pos + expected.len], expected);
    }

    fn isAtEnd(self: *const Parser) bool {
        return self.pos >= self.source.len;
    }

    fn peek(self: *const Parser) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *const Parser) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn advance(self: *Parser) u8 {
        if (self.isAtEnd()) return 0;
        const char = self.source[self.pos];
        self.pos += 1;
        return char;
    }

    fn skipWhitespace(self: *Parser) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                _ = self.advance();
            } else {
                break;
            }
        }
    }

    fn isAlphanumeric(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '-' or c == '_';
    }

    fn isVoidElement(tag: []const u8) bool {
        const voids = [_][]const u8{ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr" };
        for (voids) |v| {
            if (std.mem.eql(u8, tag, v)) return true;
        }
        return false;
    }
};

/// ## AST Traversal
/// Similar to Vaporize.zig, prints the tree structure.
fn traversePrint(node: ?*Node, writer: anytype, indent: usize) !void {
    var current = node;
    while (current) |n| {
        for (0..indent) |_| try writer.writeByte(' ');

        switch (n.tag) {
            .Root => try writer.print("[Root]\n", .{}),
            .Element => {
                const el = n.data.element;
                try writer.print("[Element: <{s}>", .{el.tag_name});
                for (el.attributes) |attr| {
                    if (attr.value) |v| {
                        try writer.print(" {s}=\"{s}\"", .{ attr.name, v });
                    } else {
                        try writer.print(" {s}", .{attr.name});
                    }
                }
                try writer.print("]\n", .{});
            },
            .Text => {
                const txt = n.data.text;
                // Sanitize newlines for cleaner printing
                var safe_content = std.array_list.Managed(u8).init(std.heap.page_allocator); // simplistic for debug print
                defer safe_content.deinit();
                for (txt.content) |c| {
                    if (c == '\n') try safe_content.appendSlice("\\n") else try safe_content.append(c);
                }
                if (txt.content.len > 0 and !isOnlyWhitespace(txt.content))
                    try writer.print("[Text: \"{s}\"]\n", .{safe_content.items});
            },
            .Comment => {
                const cmt = n.data.comment;
                try writer.print("[Comment: {s}]\n", .{cmt.content});
            },
            .Doctype => {
                const doc = n.data.doctype;
                try writer.print("[Doctype: {s}]\n", .{doc.content});
            },
        }

        try traversePrint(n.child, writer, indent + 2);
        current = n.next;
    }
}

fn isOnlyWhitespace(s: []const u8) bool {
    for (s) |c| {
        if (c != ' ' and c != '\t' and c != '\n' and c != '\r') return false;
    }
    return true;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buffer: [4096]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    var stdout = &writer.interface;
 

    const html_source =
        \\<!DOCTYPE html>
        \\<div id="app" class="container">
        \\  <h1>Hello World</h1>
        \\  <p>This is a <br> <strong>Test</strong> paragraph.</p>
        \\  <img src="logo.png" alt="Logo" />
        \\  <ul>
        \\    <li>Item 1</li>
        \\    <li>Item 2</li>
        \\  </ul>
        \\</div>
    ;

    var parser = Parser.init(allocator, html_source);
    const root = try parser.parse();


    try stdout.print("--- HTML AST ---\n", .{});
    try traversePrint(root, stdout, 0);
    stdout.flush() catch {};
}

