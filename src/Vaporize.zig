const std = @import("std");
const Fabric = @import("fabric");
const Static = Fabric.Static;
const Allocator = std.mem.Allocator;
const Text = Static.Text;
const Box = Static.Box;
const Heading = Static.Heading;
const List = Static.List;
const ListItem = Static.ListItem;
const Code = Static.Code;
const Graphic = Static.Graphic;
const CodeEditor = @import("CodeEditor.zig");

/// ## The Abstract Syntax Tree (AST)
///
/// This defines the structure of our parsed Markdown.
/// Each element (Heading, Paragraph, Text) becomes a Node.
/// Defines what kind of Markdown element a Node represents.
pub const NodeType = enum {
    Root,
    Heading,
    Paragraph,
    Text,
    Bold, // Added
    Italic, // Added
    Code, // Added
    List, // <-- NEW
    ListItem, // <-- NEW
    Image, // <-- NEW
    CodeBlock,
};

/// A single node in the AST.
/// We use a first-child/next-sibling structure for efficiency.
pub const Node = struct {
    tag: NodeType,

    // Type-specific data.
    // Using a union saves memory.
    data: union {
        heading: struct {
            level: u8,
        },
        text: struct {
            content: []const u8, // A slice of the *original* source text
        },
        // Added: Code also just holds a content slice
        code: struct {
            content: []const u8,
        },
        image: struct {
            alt_text: []const u8,
            src: []const u8,
        },
        code_block: struct {
            editor: *CodeEditor,
            content: []const u8,
            language: ?[]const u8,
        },
        none: void,
    },

    child: ?*Node, // Pointer to the first child node
    next: ?*Node, // Pointer to the next sibling node
};

/// ## The Parser
///
/// This struct holds the state of the parsing process.
/// It uses an ArenaAllocator for all Node allocations.
pub const Parser = struct {
    arena: Allocator, // The parent allocator (e.g., GPA)
    source: []const u8,
    pos: usize,

    /// Initializes a new parser with a given allocator and source text.
    pub fn init(alloc: Allocator, source: []const u8) Parser {
        return Parser{
            .arena = alloc, // Get the allocator interface
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
        var last_node: ?*Node = null;

        while (!self.isAtEnd()) {
            // 1. Skip blank lines between blocks
            self.skipBlankLines();
            if (self.isAtEnd()) break;

            // 2. Parse the next block
            const block = try self.parseBlock();

            // 3. Link the new block into the Root's child list
            if (last_node) |last| {
                last.next = block;
            } else {
                root.child = block;
            }
            last_node = block;
        }
        return root;
    }

    /// Dispatches to the correct block parser based on the current char.
    fn parseBlock(self: *Parser) !*Node {
        const char = self.peek();
        switch (char) {
            '#' => return self.parseHeading(),
            '-', '*', '+' => {
                if (self.isListItemStart()) return self.parseList();
                return self.parseParagraph();
            },
            '`' => {
                // Detect fenced code block (```lang)
                if (self.matchCodeFence()) return self.parseCodeBlock();
                return self.parseParagraph();
            },
            else => return self.parseParagraph(),
        }
    }

    fn parseCodeBlock(self: *Parser) !*Node {
        // Consume the opening ```
        _ = self.advance();
        _ = self.advance();
        _ = self.advance();

        // Capture optional language
        const lang_start = self.pos;
        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }
        const lang_end = self.pos;
        const language = std.mem.trim(u8, self.source[lang_start..lang_end], " \t");

        // Skip newline
        if (self.peek() == '\n') _ = self.advance();

        const code_start = self.pos;

        // Find closing ```
        while (!self.isAtEnd()) {
            if (self.matchCodeFence()) break;
            _ = self.advance();
        }

        const code_end = self.pos;
        const code_content = self.source[code_start..code_end];

        // Consume closing fence
        if (self.matchCodeFence()) {
            _ = self.advance();
            _ = self.advance();
            _ = self.advance();
        }

        // Create CodeBlock node
        const node = try self.createNode(.CodeBlock);
        const editor: *CodeEditor = self.arena.create(CodeEditor) catch unreachable;
        editor.*.init(&self.arena, code_content);
        node.data = .{ .code_block = .{
            .editor = editor,
            .content = code_content,
            .language = if (language.len > 0) language else null,
        } };

        return node;
    }

    fn matchCodeFence(self: *Parser) bool {
        // Check for three backticks at current position
        return self.pos + 2 < self.source.len and
            self.source[self.pos] == '`' and
            self.source[self.pos + 1] == '`' and
            self.source[self.pos + 2] == '`';
    }

    /// Parses a heading, e.g., "## My Title"
    fn parseHeading(self: *Parser) !*Node {
        var level: u8 = 0;
        while (self.peek() == '#') {
            _ = self.advance();
            level += 1;
        }

        // Skip the space after the '#'
        if (self.peek() == ' ') {
            _ = self.advance();
        }

        const line_start = self.pos;
        self.consumeLine(); // Advance pos to the end of the line
        const line_end = self.pos;

        // Get the content slice (and trim whitespace)
        const content = std.mem.trim(u8, self.source[line_start..line_end], " \t");

        // Create the Heading node
        const node = try self.createNode(.Heading);
        node.data = .{ .heading = .{ .level = level } };

        // --- MODIFIED ---
        // Instead of creating one text node, parse the inlines
        try self.parseInlines(node, content);
        // ----------------

        return node;
    }

    /// Parses a simple, single-line paragraph.
    fn parseParagraph(self: *Parser) !*Node {
        const line_start = self.pos;
        self.consumeLine(); // Advance pos to the end of the line
        const line_end = self.pos;

        // Get the content slice
        const content = std.mem.trim(u8, self.source[line_start..line_end], " \t");

        // Create the Paragraph node
        const node = try self.createNode(.Paragraph);

        // --- MODIFIED ---
        // Instead of creating one text node, parse the inlines
        try self.parseInlines(node, content);
        // ----------------

        return node;
    }

    /// Checks whether the current position starts a list item.
    /// Supports `-`, `*`, or `+` followed by a space.
    fn isListItemStart(self: *const Parser) bool {
        if (self.isAtEnd()) return false;
        const ch = self.peek();
        if (ch == '-' or ch == '*' or ch == '+') {
            if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == ' ') {
                return true;
            }
        }
        return false;
    }

    /// Parses an unordered list of items starting with '-' or '*'
    fn parseList(self: *Parser) !*Node {
        const list_node = try self.createNode(.List);
        var last_item: ?*Node = null;

        while (!self.isAtEnd() and self.isListItemStart()) {
            // Skip marker and space
            _ = self.advance(); // consume '-' or '*'
            _ = self.advance(); // consume space

            const line_start = self.pos;
            self.consumeLine();
            const line_end = self.pos;
            const content = std.mem.trim(u8, self.source[line_start..line_end], " \t");

            // Create ListItem node
            const item_node = try self.createNode(.ListItem);
            try self.parseInlines(item_node, content);

            // Link into list
            if (last_item) |last| {
                last.next = item_node;
            } else {
                list_node.child = item_node;
            }
            last_item = item_node;

            // Skip any single newline between items
            if (self.peek() == '\n') {
                _ = self.advance();
            }
        }

        return list_node;
    }

    /// --- NEW: Inline Parser ---
    ///
    /// This parses the content of a block (like a paragraph)
    /// for inline elements like bold, italic, and code.
    fn parseInlines(self: *Parser, parent: *Node, content: []const u8) !void {
        var cursor: usize = 0;
        var text_start: usize = 0;
        var last_child: ?*Node = null;

        while (cursor < content.len) {
            const char = content[cursor];
            switch (char) {
                // Check for Bold: **
                '!' => {
                    // Markdown image syntax: ![alt](src)
                    if (cursor + 1 < content.len and content[cursor + 1] == '[') {
                        const alt_start = cursor + 2;
                        if (std.mem.indexOf(u8, content[alt_start..], "]")) |alt_end_offset| {
                            const alt_end = alt_start + alt_end_offset;
                            const after_bracket = alt_end + 1;

                            if (after_bracket < content.len and content[after_bracket] == '(') {
                                const src_start = after_bracket + 1;
                                if (std.mem.indexOf(u8, content[src_start..], ")")) |src_end_offset| {
                                    const src_end = src_start + src_end_offset;

                                    // 1. Flush preceding text
                                    if (text_start < cursor) {
                                        const text_node = try self.createNode(.Text);
                                        text_node.data = .{ .text = .{ .content = content[text_start..cursor] } };
                                        if (last_child) |last| last.next = text_node else parent.child = text_node;
                                        last_child = text_node;
                                    }

                                    // 2. Create Image node
                                    const image_node = try self.createNode(.Image);
                                    image_node.data = .{
                                        .image = .{
                                            .alt_text = content[alt_start..alt_end],
                                            .src = content[src_start..src_end],
                                        },
                                    };

                                    if (last_child) |last| last.next = image_node else parent.child = image_node;
                                    last_child = image_node;

                                    // 3. Advance cursor
                                    cursor = src_end + 1;
                                    text_start = cursor;
                                    continue;
                                }
                            }
                        }
                    }

                    cursor += 1; // If not matched, continue as normal
                },
                '*' => {
                    if (cursor + 1 < content.len and content[cursor + 1] == '*') {
                        // Found start '**'. Look for end.
                        if (std.mem.indexOf(u8, content[cursor + 2 ..], "**")) |end_offset| {
                            const inner_start = cursor + 2;
                            const inner_end = cursor + 2 + end_offset;
                            const outer_end = inner_end + 2;

                            // 1. Flush any text before this
                            if (text_start < cursor) {
                                const text_node = try self.createNode(.Text);
                                text_node.data = .{ .text = .{ .content = content[text_start..cursor] } };
                                if (last_child) |last| {
                                    last.next = text_node;
                                } else {
                                    parent.child = text_node;
                                }
                                last_child = text_node;
                            }

                            // 2. Create Bold node
                            const bold_node = try self.createNode(.Bold);
                            if (last_child) |last| {
                                last.next = bold_node;
                            } else {
                                parent.child = bold_node;
                            }
                            last_child = bold_node;

                            // 3. RECURSE: Parse inlines *within* the bold tags
                            try self.parseInlines(bold_node, content[inner_start..inner_end]);

                            // 4. Move cursor past
                            cursor = outer_end;
                            text_start = cursor;
                        } else {
                            // No closing tag, treat as text
                            cursor += 1;
                        }
                    } else {
                        // Check for Italic: *
                        if (std.mem.indexOf(u8, content[cursor + 1 ..], "*")) |end_offset| {
                            const inner_start = cursor + 1;
                            const inner_end = cursor + 1 + end_offset;
                            const outer_end = inner_end + 1;

                            // 1. Flush text
                            if (text_start < cursor) {
                                const text_node = try self.createNode(.Text);
                                text_node.data = .{ .text = .{ .content = content[text_start..cursor] } };
                                if (last_child) |last| {
                                    last.next = text_node;
                                } else {
                                    parent.child = text_node;
                                }
                                last_child = text_node;
                            }

                            // 2. Create Italic node
                            const italic_node = try self.createNode(.Italic);
                            if (last_child) |last| {
                                last.next = italic_node;
                            } else {
                                parent.child = italic_node;
                            }
                            last_child = italic_node;

                            // 3. RECURSE
                            try self.parseInlines(italic_node, content[inner_start..inner_end]);

                            // 4. Move cursor
                            cursor = outer_end;
                            text_start = cursor;
                        } else {
                            // No closing tag
                            cursor += 1;
                        }
                    }
                },
                // Check for Code: `
                '`' => {
                    if (std.mem.indexOf(u8, content[cursor + 1 ..], "`")) |end_offset| {
                        const inner_start = cursor + 1;
                        const inner_end = cursor + 1 + end_offset;
                        const outer_end = inner_end + 1;

                        // 1. Flush text
                        if (text_start < cursor) {
                            const text_node = try self.createNode(.Text);
                            text_node.data = .{ .text = .{ .content = content[text_start..cursor] } };
                            if (last_child) |last| {
                                last.next = text_node;
                            } else {
                                parent.child = text_node;
                            }
                            last_child = text_node;
                        }

                        // 2. Create Code node
                        const code_node = try self.createNode(.Code);
                        code_node.data = .{ .code = .{ .content = content[inner_start..inner_end] } };
                        if (last_child) |last| {
                            last.next = code_node;
                        } else {
                            parent.child = code_node;
                        }
                        last_child = code_node;

                        // 3. NO RECURSION for code blocks.

                        // 4. Move cursor
                        cursor = outer_end;
                        text_start = cursor;
                    } else {
                        // No closing tag
                        cursor += 1;
                    }
                },
                else => {
                    // Regular character
                    cursor += 1;
                },
            }
        }

        // End of loop, flush any remaining text
        if (text_start < content.len) {
            const text_node = try self.createNode(.Text);
            text_node.data = .{ .text = .{ .content = content[text_start..content.len] } };
            if (last_child) |last| {
                last.next = text_node;
            } else {
                parent.child = text_node;
            }
            // _ = last_child; // Not needed anymore
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

    /// Consumes characters until a blank line (`\n\n`) or EOF is reached.
    /// That means it will treat single newlines as part of the same paragraph.
    fn consumeLine(self: *Parser) void {
        while (!self.isAtEnd()) {
            // Stop if we find a blank line: two consecutive newlines
            if (self.peek() == '\n') {
                // Look ahead one character
                if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == '\n') {
                    break; // Double newline -> end of paragraph
                }
            }

            _ = self.advance();
        }
    }

    /// Skips one or more blank lines.
    fn skipBlankLines(self: *Parser) void {
        while (self.peek() == '\n') {
            _ = self.advance();
        }
    }
};

/// ## AST Traversal
///
/// This function demonstrates how to "walk" or "traverse" the tree
/// recursively, printing out its structure.
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
            .Heading => |_| {
                try writer.print("[Heading (L{d})]\n", .{n.data.heading.level});
            },
            .Paragraph => try writer.print("[Paragraph]\n", .{}),
            .Text => |_| {
                try writer.print("[Text: \"{s}\"]\n", .{n.data.text.content});
            },
            .List => |_| {
                try writer.print("[List]\n", .{});
            },
            .ListItem => |_| {
                try writer.print("[ListItem]\n", .{});
            },
            // --- ADDED ---
            .Bold => try writer.print("[Bold]\n", .{}),
            .Italic => try writer.print("[Italic]\n", .{}),
            .Code => |_| {
                try writer.print("[Code: \"{s}\"]\n", .{n.data.code.content});
            },
            // -------------
        }

        // 3. Recursively traverse this node's children
        try traversePrint(n.child, writer, indent + 1);

        // 4. Move to the next sibling
        current = n.next;
    }
}

pub fn parseTraverse(markdown: []const u8, allocator: std.mem.Allocator) !void {
    // 1. Initialize the parser
    var parser = Parser.init(allocator, markdown);

    // 2. Run the parser
    const root_node = try parser.parse();

    // 3. Traverse and print the tree
    try traverse(root_node);
}

const Style = struct {
    is_bold: bool = false,
    is_italic: bool = false,
    level: u8 = 0,
    code_color: Fabric.Types.Color = .hex("#212121"),
    text_color: Fabric.Types.Color = .hex("#212121"),
    heading_color: Fabric.Types.Color = .hex("#333333"),
};

// Note the new 'style: Style' parameter!
pub fn traverse(node: ?*Node, style: Style) !void {
    var current = node;
    while (current) |n| {
        // No local 'is_bold' variables needed here.
        // We use the 'style' parameter passed into the function.

        switch (n.tag) {
            .Root => {
                // The Root node just renders its children with the default style
                try traverse(n.child, style);
            },
            .Image => |_| {
                // Render an image node (SVG or otherwise)
                const src = n.data.image.src;
                // const alt = n.data.image.alt_text;

                // You can adapt this to your Fabric component API
                // Here’s a simple conceptual render:
                // Hypothetical Image/Asset renderer
                Box.style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                })({
                    Graphic(.{ .src = src }).style(&.{
                        .size = .square_percent(70),
                    });
                });
            },
            .Heading => |_| {
                // THIS IS YOUR NEW LINE.
                // Treat it as a container. (I'm assuming you have a 'Heading' component)
                Box.style(&.{
                    // .white_space = .pre,
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(12, 12),
                    // .flex_wrap = .wrap,
                })({
                    // Now traverse the children *inside* this heading block
                    try traverse(n.child, .{
                        .is_bold = style.is_bold,
                        .is_italic = style.is_italic,
                        .level = n.data.heading.level,
                    });
                });
            },
            .Paragraph => {
                // THIS IS YOUR OTHER NEW LINE.
                // Treat it as a container. (I'm assuming you have a 'Paragraph' component)
                Box.style(&.{
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(6, 6),
                    // .flex_wrap = .wrap,
                })({
                    // Now traverse the children *inside* this paragraph block
                    try traverse(n.child, style);
                });
            },

            .List => |_| {
                List.style(&.{
                    .layout = .left_center,
                    .direction = .column,
                    .size = .w(.percent(100)),
                    .child_gap = 8,
                })({
                    // Now traverse the children *inside* this list block
                    try traverse(n.child, style);
                });
            },
            .ListItem => |_| {
                ListItem.style(&.{})({
                    // Now traverse the children *inside* this list item block
                    try traverse(n.child, style);
                });
            },

            .Text => |_| {
                // Render the text using the *passed-in* style
                if (style.level > 0) {
                    switch (style.level) {
                        1 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = style.heading_color, .font_weight = 500 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        2 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = .hex("#666666"), .font_weight = 400 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        3 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = style.heading_color, .font_weight = 600 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        4 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = style.heading_color, .font_weight = 400 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        5 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = style.heading_color, .font_weight = 300 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        6 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .text_color = style.heading_color, .font_weight = 200 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        else => {},
                    }
                } else {
                    Text(n.data.text.content).style(&.{
                        .layout = .in_line,
                        .visual = .{
                            .font_size = 16,
                            .font_weight = if (style.is_bold) 900 else 400,
                            .text_color = style.text_color,
                            .font_style = if (style.is_italic) .italic else null,
                        },
                    });
                }
                // No need to reset flags here.
            },
            .Bold => {
                // This node doesn't render itself.
                // It just tells its *children* to be bold.
                // We do this by passing a *modified* style struct.
                try traverse(n.child, .{
                    .is_bold = true, // <-- Set bold to true for children
                    .is_italic = style.is_italic,
                    .level = style.level,
                });
            },
            .Italic => {
                // Same as Bold, but for italic
                try traverse(n.child, .{
                    .is_bold = style.is_bold,
                    .is_italic = true, // <-- Set italic to true for children
                    .level = style.level,
                });
            },
            .CodeBlock => |_| {
                var editor = n.data.code_block.editor;
                editor.render(0);
            },
            .Code => |_| {
                // Code nodes render directly and ignore parent styles
                Code(n.data.code.content).style(&.{
                    .layout = .in_line,
                    .visual = .{
                        .font_size = 16,
                        .font_weight = if (style.is_bold) 900 else 400,
                        .text_color = style.code_color,
                        // You'd add italic here too, e.g.:
                        // .font_style = if (style.is_italic) .Italic else .Normal,
                    },
                    .font_family = "JetBrains Mono,Fira Code,Consolas,monospace",
                });
            },
        }

        // 3. Move to the next sibling
        // (The sibling continues with the *same* style as the current node)
        current = n.next;
    }
}

/// ## Main Function
///
/// This sets up the parser, runs it on some sample
/// Markdown, and then traverses the resulting AST.
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

    // --- MODIFIED: New sample input ---
    const markdown =
        \\# Main Heading with *Italic*
        \\
        \\This is the first paragraph with **bold text**.
        \\
        \\## Sub Heading with `inline code`
        \\
        \\This is the second paragraph.
        \\It has **nested *italic* bold** and some `code`.
    ;
    // ---------------------------------

    // 1. Initialize the parser
    var parser = Parser.init(allocator, markdown);

    // 2. Run the parser
    const root_node = try parser.parse();

    // 3. Traverse and print the tree
    try stdout.print("--- AST Traversal ---\n", .{});
    try traversePrint(root_node, stdout, 0);
    stdout.flush() catch {};
}

