/// ## An HTML Parser
///
/// This file provides a simple, single-pass HTML parser. It takes an HTML
/// string and constructs an Abstract Syntax Tree (AST) representing the
/// document's structure.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Represents a single HTML attribute (e.g., class="container").
pub const Attribute = struct {
    name: []const u8,
    value: []const u8,
};

/// Defines what kind of HTML element a Node represents.
pub const NodeType = enum {
    Root, // The top-level document node
    Element, // An HTML tag, e.g., <div>, <p>, <span>
    Text, // Plain text content between tags
};

/// A single node in the AST.
/// We use a first-child/next-sibling structure for efficiency.
pub const Node = struct {
    tag: NodeType,

    // Type-specific data.
    data: union {
        element: struct {
            tag_name: []const u8,
            attributes: std.array_list.Managed(*Attribute), // List of attributes
        },
        text: struct {
            content: []const u8, // A slice of the *original* source text
        },
        none: void, // For the Root node
    },

    child: ?*Node, // Pointer to the first child node
    next: ?*Node, // Pointer to the next sibling node
};

/// ## The Parser
///
/// This struct holds the state of the parsing process.
/// It uses an ArenaAllocator for all Node allocations.
pub const Parser = struct {
    arena: Allocator,
    source: []const u8,
    pos: usize,

    /// Initializes a new parser with a given allocator and source text.
    pub fn init(alloc: Allocator, source: []const u8) Parser {
        return Parser{
            .arena = alloc,
            .source = source,
            .pos = 0,
        };
    }

    /// Helper to create a new Node using the arena.
    fn createNode(self: *Parser, tag: NodeType) !*Node {
        const node = try self.arena.create(Node);
        node.* = .{
            .tag = tag,
            .data = .{ .none = {} }, // Default to none
            .child = null,
            .next = null,
        };
        return node;
    }

    /// --- Parsing Logic ---
    /// The main parsing function. Returns the Root node of the AST.
    pub fn parse(self: *Parser) !*Node {
        const root = try self.createNode(.Root);
        try self.parseNodes(root);
        return root;
    }

    /// Parses a sequence of sibling nodes (elements and text)
    /// and attaches them to the given parent node.
    fn parseNodes(self: *Parser, parent: *Node) !void {
        var last_child: ?*Node = null;

        while (!self.isAtEnd()) {
            self.skipWhitespace();
            if (self.isAtEnd()) break;

            if (self.peek() == '<') {
                // Check if this is a closing tag
                if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '/') {
                    // This is a closing tag, so we're done parsing
                    // children for this parent.
                    break;
                }

                // This is an opening tag. Parse the element.
                const element_node = try self.parseElement();

                // Append to parent's child list (O(1))
                if (last_child) |last| {
                    last.next = element_node;
                } else {
                    parent.child = element_node;
                }
                last_child = element_node;
            } else {
                // This is text.
                if (try self.parseText()) |text_node| {
                    // Append to parent's child list (O(1))
                    if (last_child) |last| {
                        last.next = text_node;
                    } else {
                        parent.child = text_node;
                    }
                    last_child = text_node;
                }
            }
        }
    }

    /// Parses a single text node.
    /// Returns null if the text node is only whitespace.
    fn parseText(self: *Parser) !?*Node {
        const start = self.pos;
        while (!self.isAtEnd() and self.peek() != '<') {
            _ = self.advance();
        }
        const content = std.mem.trim(u8, self.source[start..self.pos], " \t\n\r");

        // Don't create nodes for just whitespace
        if (content.len == 0) return null;

        const node = try self.createNode(.Text);
        node.data = .{ .text = .{ .content = content } };
        return node;
    }

    /// Parses a single HTML element, including its tag,
    /// attributes, and any children.
    fn parseElement(self: *Parser) !*Node {
        // 1. Consume '<'
        _ = self.advance();

        // 2. Parse tag name
        const tag_name = self.consumeTagName();

        // 3. Create the node
        const node = try self.createNode(.Element);
        node.data = .{ .element = .{
            .tag_name = tag_name,
            .attributes = std.array_list.Managed(*Attribute).init(self.arena),
        } };

        // 4. Parse attributes
        try self.parseAttributes(node);

        self.skipWhitespace();

        // 5. Check for self-closing or void tag
        if (self.peek() == '/') {
            // Self-closing tag: <... />
            _ = self.advance(); // Consume '/'
            if (self.peek() == '>') _ = self.advance(); // Consume '>'
            return node; // No children
        }

        if (self.peek() == '>') {
            _ = self.advance(); // Consume '>'
        }

        // Check for "void elements" (e.g., <img>, <br>) which don't have
        // children or a closing tag.
        if (isVoidElement(tag_name)) {
            return node;
        }

        // 6. Parse children recursively
        try self.parseNodes(node);

        // 7. Consume closing tag
        self.skipWhitespace();
        if (self.peek() == '<') _ = self.advance();
        if (self.peek() == '/') _ = self.advance();
        _ = self.consumeTagName(); // TODO: We could validate this matches tag_name
        if (self.peek() == '>') _ = self.advance();

        return node;
    }

    /// Parses the attribute list of an element.
    fn parseAttributes(self: *Parser, node: *Node) !void {
        while (!self.isAtEnd()) {
            self.skipWhitespace();
            const char = self.peek();

            // End of attributes list
            if (char == '>' or char == '/') break;

            // Parse attribute name
            const name = self.consumeAttrName();
            if (name.len == 0) break; // Should not happen

            self.skipWhitespace();

            var value: []const u8 = ""; // Default for boolean attributes

            // Check for a value
            if (self.peek() == '=') {
                _ = self.advance(); // Consume '='
                self.skipWhitespace();
                const quote = self.advance(); // Consume quote ('"' or '\'')
                if (quote != '"' and quote != '\'') {
                    std.debug.print("WARN: Unquoted attribute, skipping\n", .{});
                    continue;
                }
                const value_start = self.pos;
                while (!self.isAtEnd() and self.peek() != quote) {
                    _ = self.advance();
                }
                value = self.source[value_start..self.pos];
                _ = self.advance(); // Consume closing quote
            }

            // Add attribute to list
            const attr = try self.arena.create(Attribute);
            attr.* = .{ .name = name, .value = value };
            try node.data.element.attributes.append(attr);
        }
    }

    /// --- Utility Functions ---
    fn isAtEnd(self: *const Parser) bool {
        return self.pos >= self.source.len;
    }

    /// Returns the character at the current position without consuming it.
    fn peek(self: *const Parser) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    /// Consumes and returns the character at the current position.
    fn advance(self: *Parser) u8 {
        if (self.isAtEnd()) return 0;
        const char = self.source[self.pos];
        self.pos += 1;
        return char;
    }

    /// Skips whitespace characters.
    fn skipWhitespace(self: *Parser) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\t', '\n', '\r' => _ = self.advance(),
                else => return,
            }
        }
    }

    /// Consumes a valid tag name.
    fn consumeTagName(self: *Parser) []const u8 {
        const start = self.pos;
        while (!self.isAtEnd()) {
            // This is a simplified check
            switch (self.peek()) {
                'a'...'z', 'A'...'Z', '0'...'9' => _ = self.advance(),
                else => return self.source[start..self.pos],
            }
        }
        return self.source[start..self.pos];
    }

    /// Consumes a valid attribute name.
    fn consumeAttrName(self: *Parser) []const u8 {
        const start = self.pos;
        while (!self.isAtEnd()) {
            // This is a simplified check
            switch (self.peek()) {
                'a'...'z', 'A'...'Z', '0'...'9', '-' => _ = self.advance(),
                else => return self.source[start..self.pos],
            }
        }
        return self.source[start..self.pos];
    }
};

/// Helper to check for void elements that don't need a closing tag.
fn isVoidElement(tag_name: []const u8) bool {
    const void_tags = [_][]const u8{
        "area",  "base", "br",   "col",   "embed",  "hr",    "img",
        "input", "link", "meta", "param", "source", "track", "wbr",
    };
    for (void_tags) |tag| {
        if (std.mem.eql(u8, tag_name, tag)) return true;
    }
    return false;
}

/// ## AST Traversal
///
/// This function "walks" the tree recursively, printing its structure.
fn traversePrint(node: ?*Node, writer: *std.io.Writer, indent: usize) !void {
    var current = node;
    while (current) |n| {
        // 1. Print indentation
        for (0..indent) |_| {
            try writer.writeByte(' ');
        }

        // 2. Print the current node's info
        switch (n.tag) {
            .Root => try writer.print("[Root]\n", .{}),
            .Text => {
                try writer.print("[Text: \"{s}\"]\n", .{n.data.text.content});
            },
            .Element => {
                try writer.print("[Element: <{s}", .{n.data.element.tag_name});
                // Print attributes
                for (n.data.element.attributes.items) |attr| {
                    try writer.print(" {s}=\"{s}\"", .{ attr.name, attr.value });
                }
                try writer.print("]\n", .{});
            },
        }

        // 3. Recursively traverse this node's children
        try traversePrint(n.child, writer, indent + 2);
        // 4. Move to the next sibling
        current = n.next;
    }
}

/// ## Main Function
///
/// This sets up the parser, runs it on some sample
/// HTML, and then traverses the resulting AST.
pub fn main() !void {
    // Note: Your buffered writer setup is slightly different from std.io.getStdOut()
    // but it's perfectly valid.
    var buffer: [4096]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    var stdout = &writer.interface;
    // const stdout = &stdout_buffer;

    // Use a General Purpose Allocator as the "parent"
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    defer arena.deinit();

    // --- Sample HTML input ---
    const html =
        \\<div id="main" class="container">
        \\    <h1>Hello, World!</h1>
        \\    <p>This is a <b>test</b> parser.</p>
        \\    <img src="image.png" alt="An image" />
        \\</div>
    ;

    // 1. Initialize the parser
    var parser = Parser.init(allocator, html);

    // 2. Run the parser
    const root_node = try parser.parse();

    // 3. Traverse and print the tree
    try stdout.print("--- HTML AST Traversal ---\n", .{});
    try traversePrint(root_node, stdout, 0);
}

