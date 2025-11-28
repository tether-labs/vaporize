const std = @import("std");
const Vapor = @import("vapor");
const Static = Vapor.Static;
const Allocator = std.mem.Allocator;
const Text = Static.Text;
const Box = Static.Box;
const Heading = Static.Heading;
const List = Static.List;
const ListItem = Static.ListItem;
const Code = Static.Code;
const Graphic = Static.Graphic;
const Image = Static.Image;
const Link = Static.Link;
const Section = Static.Section;
const CodeEditor = @import("CodeEditor.zig");
const Snippet = @import("Snippet.zig");
const ListStyle = Vapor.Types.ListStyle;
const Button = Static.Button;
const TextField = Vapor.TextField;
const Label = Vapor.Label;
const Form = Vapor.Form;
const SubmitButton = Vapor.SubmitButton;

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
    Svg, // <-- NEW
    Image, // <-- NEW
    CodeBlock,
    Section,
    RunBlock,
    Snippet,
    Link,
    Box,
    Button,
    TextField,
    Form,
};

// // Enum definition for CSS list-style-type property
// pub const ListStyle = enum(u8) {
//     default,
//     none, // No bullet or marker
//     disc, // Filled circle (default for unordered lists)
//     circle, // Open circle
//     square, // Square marker
//     decimal, // Decimal numbers (default for ordered lists)
//     decimal_leading_zero, // Decimal numbers with a leading zero (e.g. 01, 02, 03, ...)
//     lower_roman, // Lowercase roman numerals (i, ii, iii, ...)
//     upper_roman, // Uppercase roman numerals (I, II, III, ...)
//     lower_alpha, // Lowercase alphabetic (a, b, c, ...)
//     upper_alpha, // Uppercase alphabetic (A, B, C, ...)
//     lower_greek, // Lowercase Greek letters (α, β, γ, ...)
//     armenian, // Armenian numbering
//     georgian, // Georgian numbering
//     inherit, // Inherits from parent element
//     initial, // Resets to the default value
//     revert, // Reverts to the inherited value if explicitly changed
//     unset, // Resets to inherited or initial value
// };
//
/// --- NEW: Recursive List Parsing ---
const ListMarker = enum { @"-", @"*", @"+" };
const UnorderedMarker = enum { @"-", @"*", @"+" };
// Represents a detected ordered list marker
const OrderedMarkerInfo = struct {
    style: ListStyle, // .decimal, .lower_alpha, etc.
    delimiter: u8, // '.' or ')'
};

// A marker is a "sum type" or "tagged union".
// It can be EITHER Unordered OR Ordered.
const Marker = union(enum) {
    Unordered: UnorderedMarker,
    Ordered: OrderedMarkerInfo,
};

// The LineInfo struct is updated to use the new Marker union
const LineInfo = struct {
    indent: usize,
    marker: ?Marker = null, // null if not a list item
    content_start_pos: usize, // absolute position in self.source
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
        snippet: struct {
            content: []const u8,
            ptr: *Snippet,
        },
        section: struct {
            id: []const u8,
        },
        run_block: struct {
            src: []const u8,
        },
        link: struct {
            href: []const u8,
            title: []const u8,
        },
        list: struct {
            list_type: ListStyle,
        },
        button: struct {
            text: []const u8,
            on_click: ?*const fn () void,
        },
        textfield: struct {
            title: []const u8,
            type: Vapor.Types.InputTypes = .string,
            err: ?*ValidationResult = null,
            err_index: usize = 0,
        },
        none: void,
    },

    child: ?*Node, // Pointer to the first child node
    next: ?*Node, // Pointer to the next sibling node
};

pub const Self = @This();
allocator: Allocator,
root: *Node,
style: Style,

pub fn init(allocator: Allocator, style: Style) !Self {
    const root = try allocator.create(Node);
    root.* = .{
        .tag = .Root,
        .data = .{ .none = {} }, // Default to none
        .child = null,
        .next = null,
    };
    return Self{
        .allocator = allocator,
        .root = root,
        .style = style,
    };
}

/// Helper to create a new Node using the arena.
fn createNode(self: *Self, tag: NodeType) !*Node {
    const node = try self.allocator.create(Node);
    node.* = .{
        .tag = tag,
        .data = .{ .none = {} }, // Default to none
        .child = null,
        .next = null,
    };
    return node;
}

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
        var last_section: ?*Node = null;

        while (!self.isAtEnd()) {
            self.skipBlankLines();
            if (self.isAtEnd()) break;

            const block = try self.parseBlock();

            switch (block.tag) {
                .Section => {
                    // New section begins
                    if (last_section) |section| {
                        section.next = block;
                    } else {
                        root.child = block;
                    }
                    last_section = block;
                },
                else => {
                    // Add block to current section (or root if no section yet)
                    const parent = last_section orelse root;
                    self.attachChild(parent, block);
                },
            }
        }

        return root;
    }

    /// Attaches `child` to the end of `parent`'s child list.
    fn attachChild(_: *Parser, parent: *Node, child: *Node) void {
        if (parent.child) |first| {
            var last = first;
            while (last.next) |next| {
                last = next;
            }
            last.next = child;
        } else {
            parent.child = child;
        }
    }

    /// Dispatches to the correct block parser based on the current char.
    fn parseBlock(self: *Parser) !*Node {
        // --- MODIFIED ---
        // Check for a list item *first*, before checking other block types.
        // This handles '-', '*', '+', '1.', 'a.', etc., all at once.
        if (self.isListItemStart()) {
            return try self.parseList(0);
        }
        // --- END MODIFICATION ---

        const char = self.peek();
        switch (char) {
            '{' => return self.parseSection(),
            '#' => return self.parseHeading(),
            '%' => return self.parseSnippet(),
            // The '-', '*', '+' case is now handled above
            '-', '*', '+' => {
                // MODIFIED:
                if (self.isListItemStart()) return try self.parseList(0);
                return self.parseParagraph();
                // },
            },
            '`' => {
                // Detect fenced code block (```lang)
                if (self.matchCodeFence()) return self.parseCodeBlock();
                return self.parseParagraph();
            },
            '@' => {
                // Detect fenced code block (```lang)
                _ = self.advance();
                return self.parseRunBlock();
            },

            else => return self.parseParagraph(),
        }
    }

    fn parseRunBlock(self: *Parser) !*Node {
        // Capture optional language
        const lang_start = self.pos;
        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }
        const lang_end = self.pos;
        const src = std.mem.trim(u8, self.source[lang_start..lang_end], " \t");

        // Create CodeBlock node
        const node = try self.createNode(.RunBlock);
        node.data = .{ .run_block = .{
            .src = src,
        } };

        return node;
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
        node.data = .{
            .code_block = .{
                .editor = editor,
                .content = code_content,
                .language = if (language.len > 0) language else null,
            },
        };

        return node;
    }

    fn matchCodeFence(self: *Parser) bool {
        // Check for three backticks at current position
        return self.pos + 2 < self.source.len and
            self.source[self.pos] == '`' and
            self.source[self.pos + 1] == '`' and
            self.source[self.pos + 2] == '`';
    }

    /// Consumes characters until a single newline or EOF is reached.
    /// Returns the content slice *without* the newline.
    /// Advances parser *past* the newline if found.
    fn consumeSingleLine(self: *Parser) []const u8 {
        const line_start = self.pos;

        while (!self.isAtEnd() and self.peek() != '\n') {
            _ = self.advance();
        }

        const line_end = self.pos;

        // Skip the newline itself
        if (!self.isAtEnd() and self.peek() == '\n') {
            _ = self.advance();
        }

        return std.mem.trim(u8, self.source[line_start..line_end], " \t");
    }

    fn parseSection(self: *Parser) !*Node {
        while (self.peek() != '#') {
            _ = self.advance();
        }
        _ = self.advance();
        const content = self.consumeSingleLine();

        // Create the Section node
        const node = try self.createNode(.Section);
        node.data = .{ .section = .{ .id = content[0 .. content.len - 1] } };

        // 3. Link the new block into the Root's child list
        return node;
    }

    /// Parses a heading, e.g., "## My Title"
    fn parseSnippet(self: *Parser) !*Node {
        _ = self.advance();

        const content = self.consumeSingleLine();
        // Create the Snippet node
        const snippet = self.arena.create(Snippet) catch unreachable;
        snippet.init();
        const node = try self.createNode(.Snippet);
        node.data = .{
            .snippet = .{
                .content = content,
                .ptr = snippet,
            },
        };
        return node;
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

        const content = self.consumeSingleLine();

        // Create the Heading node
        const node = try self.createNode(.Heading);
        node.data = .{ .heading = .{ .level = level } };

        // --- MODIFIED ---
        // Instead of creating one text node, parse the inlines
        try self.parseInlines(node, content);
        // ----------------

        Vapor.print("content: {s}\n", .{node.child.?.data.text.content});
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
    /// Helper to get indentation and list marker info for the current line.
    fn getLineInfo(self: *const Parser) LineInfo {
        var indent: usize = 0;
        var i = self.pos;

        // 1. Calculate indentation (supports spaces and tabs)
        while (i < self.source.len) {
            const char = self.source[i];
            if (char == ' ') {
                indent += 1;
                i += 1;
            } else if (char == '\t') {
                // Treat a tab as expanding to the next 4-space stop
                indent = (indent / 4 + 1) * 4;
                i += 1;
            } else {
                break;
            }
        }

        const content_start_idx = i;

        // 2. Check for Unordered Markers ('-', '*', '+')
        if (content_start_idx + 1 < self.source.len and self.source[content_start_idx + 1] == ' ') {
            const marker_char = self.source[content_start_idx];
            const marker: ?UnorderedMarker = switch (marker_char) {
                '-' => .@"-",
                '*' => .@"*",
                '+' => .@"+",
                else => null,
            };

            if (marker) |m| {
                return .{
                    .indent = indent,
                    .marker = .{ .Unordered = m },
                    .content_start_pos = content_start_idx + 2, // Skip marker and space
                };
            }
        }

        // 3. Check for Ordered Markers (e.g., "1.", "a.", "I)")
        var marker_end = content_start_idx;
        while (marker_end < self.source.len and
            std.ascii.isAlphanumeric(self.source[marker_end]))
        {
            marker_end += 1;
        }

        if (marker_end > content_start_idx and // found an alphanumeric part
            marker_end + 1 < self.source.len and // has room for delim + space
            (self.source[marker_end] == '.' or self.source[marker_end] == ')') and
            self.source[marker_end + 1] == ' ')
        {
            // We have a valid ordered marker, e.g., "1." or "a."
            const delim_char = self.source[marker_end];
            const marker_content = self.source[content_start_idx..marker_end];

            // Now, determine style
            var style: ListStyle = .decimal; // Default
            var all_digits = true;
            for (marker_content) |c| {
                if (!std.ascii.isDigit(c)) all_digits = false;
            }

            if (all_digits) {
                style = .decimal;
            } else if (marker_content.len == 1) {
                const char = marker_content[0];
                if (char == 'i') style = .lower_roman else if (char == 'I') style = .upper_roman else if (std.ascii.isLower(char)) style = .lower_alpha else if (std.ascii.isUpper(char)) style = .upper_alpha else style = .decimal; // Failsafe
            } else {
                // Not a simple case (e.g., "iv.", "ab.").
                // For this parser, we'll just base it on the first char.
                const char = marker_content[0];
                if (std.ascii.isLower(char)) style = .lower_alpha else if (std.ascii.isUpper(char)) style = .upper_alpha else style = .decimal; // Failsafe
            }

            return .{
                .indent = indent,
                .marker = .{ .Ordered = .{
                    .style = style,
                    .delimiter = delim_char,
                } },
                .content_start_pos = marker_end + 2, // skip delim and space
            };
        }

        // Not a list item
        return .{ .indent = indent, .marker = null, .content_start_pos = content_start_idx };
    }

    /// Checks whether the current position starts a list item.
    fn isListItemStart(self: *const Parser) bool {
        const info = self.getLineInfo();
        // We only check for a marker. parseList will handle indentation.
        return info.marker != null;
    }

    /// Parses an unordered list, recursively handling nested lists.
    /// `min_indent` is the indentation level required to be part of the *current* list.
    fn parseList(self: *Parser, min_indent: usize) !*Node {
        // --- PEEK at the first item to determine list type ---
        const first_line_info = self.getLineInfo();
        const first_marker = first_line_info.marker orelse {
            // This should be impossible if called from parseBlock,
            // but return an empty list node just in case.
            return self.createNode(.List);
        };

        // --- Determine the list style from the *first* item ---
        const list_style: ListStyle = switch (first_marker) {
            .Unordered => |marker| switch (marker) {
                .@"-" => .disc,
                .@"*" => .circle, // Use .circle as you had hardcoded [cite: 106]
                .@"+" => .square,
            },
            .Ordered => |info| info.style,
        };

        const list_node = try self.createNode(.List);
        list_node.data = .{
            .list = .{
                .list_type = list_style, // <-- Set the type dynamically!
            },
        };
        var last_item: ?*Node = null;

        while (!self.isAtEnd()) {
            self.skipBlankLines(); // Skip blank lines between items [cite: 107]
            if (self.isAtEnd()) break;

            const info = self.getLineInfo();

            // Check if this line is a valid list item for this level
            if (info.marker == null or info.indent < min_indent) {
                // Not a list item, or it's de-dented.
                // This means the current list is done. [cite: 109-110]
                break;
            }

            // --- NEW CHECK: Stop if marker type *changes* ---
            // (e.g., from "1." to "-") at the same indent level
            if (info.marker) |current_marker| {
                // Check if both markers are of the same kind (both Unordered or both Ordered)
                const is_same_type = (first_marker == .Unordered and current_marker == .Unordered) or
                    (first_marker == .Ordered and current_marker == .Ordered);

                if (!is_same_type and info.indent == min_indent) {
                    // Same indentation level, but changed from ordered to unordered
                    // or vice-versa. This is a new list.
                    break;
                }
            }
            // --- End New Check ---

            // --- It's a valid item for this list ---

            // 1. Consume the line's content
            self.pos = info.content_start_pos;
            const content = self.consumeSingleLine();

            // 2. Create the ListItem node
            const item_node = try self.createNode(.ListItem);
            try self.parseInlines(item_node, content);

            // 3. Link this item into the list
            if (last_item) |last| {
                last.next = item_node;
            } else {
                list_node.child = item_node;
            }
            last_item = item_node;

            // 4. --- Check for a nested list ---
            // Peek at the *next* line to see if it's a nested list
            if (!self.isAtEnd()) {
                const next_line_info = self.getLineInfo();

                if (next_line_info.marker != null and next_line_info.indent > info.indent) {
                    // The next line is a list item and it's *more* indented.
                    // This means it's a nested list belonging to the *current item*.
                    const nested_list = try self.parseList(next_line_info.indent);

                    // Attach the nested list as a child of the *current item*
                    self.attachChild(item_node, nested_list);
                }
            }
        }
        return list_node;
    }

    /// --- NEW: Inline Parser ---
    ///
    /// This parses the content of a block (like a paragraph)
    /// for inline elements like bold, italic, and code.
    // fn parseInlines(self: *Parser, parent: *Node, content: []const u8) !void {

    /// Checks whether the current position starts a list item.
    /// Supports `-`, `*`, or `+` followed by a space.
    // fn isListItemStart(self: *const Parser) bool {
    //     if (self.isAtEnd()) return false;
    //     const ch = self.peek();
    //     if (ch == '-' or ch == '*' or ch == '+') {
    //         if (self.pos + 1 < self.source.len and self.source[self.pos + 1] == ' ') {
    //             return true;
    //         }
    //     }
    //     return false;
    // }
    //
    // /// Parses an unordered list of items starting with '-' or '*'
    // fn parseList(self: *Parser) !*Node {
    //     const list_node = try self.createNode(.List);
    //     var last_item: ?*Node = null;
    //
    //     while (!self.isAtEnd() and self.isListItemStart()) {
    //         // Skip marker and space
    //         _ = self.advance(); // consume '-' or '*'
    //         _ = self.advance(); // consume space
    //
    //         const line_start = self.pos;
    //         self.consumeLine();
    //         const line_end = self.pos;
    //         const content = std.mem.trim(u8, self.source[line_start..line_end], " \t");
    //
    //         // Create ListItem node
    //         const item_node = try self.createNode(.ListItem);
    //         try self.parseInlines(item_node, content);
    //
    //         // Link into list
    //         if (last_item) |last| {
    //             last.next = item_node;
    //         } else {
    //             list_node.child = item_node;
    //         }
    //         last_item = item_node;
    //
    //         // Skip any single newline between items
    //         if (self.peek() == '\n') {
    //             _ = self.advance();
    //         }
    //     }
    //
    //     return list_node;
    // }

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
                '[' => {
                    // Markdown image syntax: ![alt](src)
                    const alt_start = cursor + 1;
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
                                var image_node: *Node = undefined;
                                image_node = try self.createNode(.Link);
                                image_node.data = .{
                                    .link = .{
                                        .title = content[alt_start..alt_end],
                                        .href = content[src_start..src_end],
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

                    cursor += 1; // If not matched, continue as normal
                },

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
                                    const src = content[src_start..src_end];
                                    var image_node: *Node = undefined;
                                    if (std.mem.indexOf(u8, src, ".svg") != null) {
                                        image_node = try self.createNode(.Svg);
                                    } else {
                                        image_node = try self.createNode(.Image);
                                    }
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
                '_' => {
                    // Check for Italic: _
                    if (std.mem.indexOf(u8, content[cursor + 1 ..], "_")) |end_offset| {
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
            else => {},
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
    _ = try parser.parse();

    // 3. Traverse and print the tree
    // try traverse(root_node);
}

const Style = struct {
    // is_bold: bool = false,
    // is_italic: bool = false,
    // level: u8 = 0,
    code_style: Vapor.Types.Style = .{ .visual = .font(16, null, .hex("#212121")) },
    text_style: Vapor.Types.Style = .{ .visual = .font(16, null, .hex("#212121")) },

    is_bold: bool = false,
    is_italic: bool = false,
    level: u8 = 0,
    code_color: Vapor.Types.Color = .hex("#212121"),
    text_color: Vapor.Types.Color = .hex("#212121"),
    heading_color: Vapor.Types.Color = .hex("#333333"),
    // list_style: Vapor.Types.Style = .{
    //     .layout = .left_center,
    //     .direction = .column,
    //     .size = .w(.percent(100)),
    //     .child_gap = 8,
    // },
    // button_style: Vapor.Types.Style = .{
    //     .layout = .in_line,
    //     .size = .w(.percent(100)),
    //     .padding = .tb(6, 6),
    //     .visual = .{ .border = .sharp(.solid, .hex("#FF0000")) },
    // },
    // struct_style: Vapor.Types.Style = .{
    //     .size = .w(.percent(100)),
    //     .padding = .tb(6, 6),
    // },
};

pub fn TaggedFunction(comptime T: type) type {
    return struct {
        tag: []const u8,
        args: ?T = null,
        function: ?*const fn (T) void = null,
        no_args_function: ?*const fn () void = null,
    };
}

fn getUnderlyingType(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .optional => std.meta.Child(T),
        else => T,
    };
}

fn getUnderlyingValue(comptime T: type, comptime OT: type, v: OT) T {
    return switch (@typeInfo(OT)) {
        .optional => v.?,
        else => v,
    };
}

pub fn compile(self: *Self, value: anytype) !void {
    const root = try self.createNode(.Root);
    current_parent = null;
    try self.generate(root, "root", value);
    try traverse(root, self.style, void, null);
}

fn attachChild(_: *Self, parent: *Node, child: *Node) void {
    if (parent.child) |first| {
        var last = first;
        while (last.next) |next| {
            last = next;
        }
        last.next = child;
    } else {
        parent.child = child;
    }
}

var current_parent: ?*Node = null;
pub fn comptimeForm(self: *Self, comptime T: type) type {
    return struct {
        const Vaporize = @This();

        const InnerForm = struct {
            form_data: T,
            validation_error: ?*ValidationResult = null,
        };

        self: *Self,
        inner_form: *InnerForm,

        pub fn init(vaporize: *Vaporize) !void {
            const allocator = Vapor.arena(.persist);
            const inner_self = allocator.create(Self) catch unreachable;
            inner_self.* = try Self.init(allocator, self.style);
            const inner_form = try allocator.create(InnerForm);
            const validation_error = allocator.create(ValidationResult) catch unreachable;
            validation_error.* = ValidationResult.init(allocator);
            inner_form.* = .{
                .form_data = T{},
                .validation_error = validation_error,
            };
            vaporize.inner_form = inner_form;
            vaporize.* = .{
                .self = inner_self,
                .inner_form = inner_form,
            };
        }
        pub fn generate(vaporize: *Vaporize) !void {
            const root = try vaporize.self.createNode(.Form);
            current_parent = null;
            vaporize.self.root = root;
            vaporize.self.generateForm(vaporize.self.root, "root", T, {}, T, vaporize.inner_form) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
            };
        }
        pub fn render(vaporize: *const Vaporize) void {
            traverseForm(vaporize.self.root, vaporize.self.style, void, null, vaporize.inner_form) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
            };
        }
    };
}

pub fn compileForm(self: *Self, comptime T: type) type {
    const allocator = Vapor.arena(.persist);
    const root = try self.createNode(.Form);
    current_parent = null;
    self.root = root;
    const InnerForm = struct {
        form_data: T,
        validation_error: ?*ValidationResult = null,
    };
    const inner_form = try allocator.create(InnerForm);
    var validation_error = ValidationResult.init(self.allocator);
    inner_form.* = .{
        .form_data = T{},
        .validation_error = &validation_error,
    };
    try self.generateForm(self.root, "root", T, {}, T, inner_form);

    const FormStruct = struct {
        const Vaporize = @This();
        self: *Self,
        inner_form: *InnerForm,
        fn render(vaporize: *const Vaporize) !void {
            try traverseForm(vaporize.self.root, vaporize.self.style, void, null, vaporize.inner_form);
        }
    };
    const form_struct = FormStruct{
        .self = self,
        .inner_form = inner_form,
    };
    return form_struct;
}

pub fn CreateForm(comptime T: type) type {
    return struct {
        var validations: T.__validations = undefined;
    };
}

pub const Validation = struct {
    err: ?[]const u8 = null,
    // Length constraints (for strings)
    min: ?usize = null,
    max: ?usize = null,

    // Value constraints (for numbers)
    min_value: ?i64 = null,
    max_value: ?i64 = null,

    // Field type validation
    field_type: ?Vapor.Types.InputTypes = .string,

    // Content validation
    includes: ?[]const u8 = null,
    excludes: ?[]const u8 = null,
    pattern: ?[]const u8 = null,

    // Flags
    required: bool = false,
    trim: bool = true, // Trim whitespace before validation

    // Custom validation function
    custom: ?*const fn (value: anytype) bool = null,
};

// Validation error structure
pub const ValidationError = struct {
    field: []const u8,
    message: []const u8,
};

// Validation result
pub const ValidationResult = struct {
    valid: bool = true,
    errors: std.array_list.Managed(ValidationError),

    pub fn init(allocator: std.mem.Allocator) ValidationResult {
        return .{
            .errors = std.array_list.Managed(ValidationError).init(allocator),
        };
    }

    pub fn deinit(self: *ValidationResult) void {
        self.errors.deinit();
    }
};

// Main validation function
pub fn validateForm(form_closure: anytype, evt: *Vapor.Event) void {
    evt.preventDefault();

    if (!@hasField(@typeInfo(@TypeOf(form_closure)).pointer.child, "form_data")) {
        Vapor.print("form_data not defined\n", .{});
        return;
    }
    const form_value = @field(form_closure, "form_data");

    const result: *ValidationResult = form_closure.validation_error orelse {
        Vapor.printErr("validation_error not defined\n", .{});
        return;
    };
    errdefer result.deinit();

    // Check if validations exist
    if (!@hasDecl(@TypeOf(form_value), "__validations")) {
        Vapor.print("No validations defined for form\n", .{});
        return;
    }

    const validations = @TypeOf(form_value).__validations;

    // Parse form data
    const parsed_form = evt.formData(&form_value) orelse {
        result.valid = false;
        result.errors.append(.{
            .field = "form",
            .message = "Failed to parse form data",
        }) catch {};
        return;
    };

    // Get form fields
    const FormType = @TypeOf(parsed_form);
    const form_fields = @typeInfo(FormType).@"struct".fields;

    result.errors.clearRetainingCapacity();

    // Validate each field
    inline for (form_fields) |field| {
        const field_name = field.name;

        // Skip if no validation for this field
        if (!@hasField(@TypeOf(validations), field_name)) continue;

        const field_value = @field(parsed_form, field_name);
        const validation = @field(validations, field_name);

        // Validate the field
        const field_valid = validateField(field_name, field_value, validation, result) catch |err| {
            Vapor.printErr("Validation failed: {any}\n", .{err});
            return;
        };
        if (!field_valid) {
            result.valid = false;
        } else {
            result.errors.append(.{
                .field = field_name,
                .message = "",
            }) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
            };
        }
    }

    Vapor.printErr("validation_error: {any}\n", .{form_closure.validation_error.?.errors.items.len});
    // for (result.errors.items) |err| {
    //     Vapor.printErr("{s}: {s}\n", .{ err.field, err.message });
    // }
    //
    // for (form_closure.validation_error.?.errors.items) |err| {
    //     Vapor.printErr("{s}: {s}\n", .{ err.field, err.message });
    // }

    Vapor.cycle();

    return;
}

// Validate individual field
fn validateField(
    field_name: []const u8,
    value: anytype,
    validation: Validation,
    result: *ValidationResult,
) !bool {
    var is_valid = true;
    const ValueType = @TypeOf(value);

    // Helper to add error
    const addError = struct {
        fn add(res: *ValidationResult, fname: []const u8, val: Validation, msg: []const u8) !void {
            const error_msg = val.err orelse msg;
            try res.errors.append(.{
                .field = fname,
                .message = error_msg,
            });
        }
    }.add;

    // Required check
    if (validation.required) {
        const is_empty = switch (@typeInfo(ValueType)) {
            .pointer => |ptr| switch (ptr.size) {
                .slice => value.len == 0,
                else => false,
            },
            .optional => value == null,
            else => false,
        };

        if (is_empty) {
            try addError(result, field_name, validation, "This field is required");
            return false;
        }
    }

    // Type-specific validation
    switch (@typeInfo(ValueType)) {
        .pointer => |ptr| switch (ptr.size) {
            .slice => {
                // String validations
                var str_value = value;

                // Trim if requested
                if (validation.trim) {
                    str_value = std.mem.trim(u8, str_value, " \t\n\r");
                }

                // Min length
                if (validation.min) |min| {
                    if (str_value.len < min) {
                        const msg = try std.fmt.allocPrint(
                            result.errors.allocator,
                            "Must be at least {d} characters",
                            .{min},
                        );
                        try addError(result, field_name, validation, msg);
                        is_valid = false;
                    }
                }

                // Max length
                if (validation.max) |max| {
                    if (str_value.len > max) {
                        const msg = try std.fmt.allocPrint(
                            result.errors.allocator,
                            "Must be at most {d} characters",
                            .{max},
                        );
                        try addError(result, field_name, validation, msg);
                        is_valid = false;
                    }
                }

                // Includes check
                if (validation.includes) |substr| {
                    if (std.mem.indexOf(u8, str_value, substr) == null) {
                        const msg = try std.fmt.allocPrint(
                            result.errors.allocator,
                            "Must contain '{s}'",
                            .{substr},
                        );
                        try addError(result, field_name, validation, msg);
                        is_valid = false;
                    }
                }

                // Excludes check
                if (validation.excludes) |substr| {
                    if (std.mem.indexOf(u8, str_value, substr) != null) {
                        const msg = try std.fmt.allocPrint(
                            result.errors.allocator,
                            "Must not contain '{s}'",
                            .{substr},
                        );
                        try addError(result, field_name, validation, msg);
                        is_valid = false;
                    }
                }

                // Field type validation
                if (validation.field_type) |field_type| {
                    const type_valid = switch (field_type) {
                        .email => validateEmail(str_value),
                        .password => validatePassword(str_value),
                        // .url => validateUrl(str_value),
                        // .phone => validatePhone(str_value),
                        // .alphanumeric => validateAlphanumeric(str_value),
                        else => true,
                    };

                    if (!type_valid) {
                        const msg = switch (field_type) {
                            .email => "Must be a valid email address",
                            .password => "Password must be at least 8 characters with uppercase, lowercase, and numbers",
                            // .url => "Must be a valid URL",
                            // .phone => "Must be a valid phone number",
                            // .alphanumeric => "Must contain only letters and numbers",
                            else => "Invalid format",
                        };
                        try addError(result, field_name, validation, msg);
                        is_valid = false;
                    }
                }
            },
            else => {},
        },

        .int, .comptime_int => {
            // Number validations
            if (validation.min_value) |min| {
                if (value < min) {
                    const msg = try std.fmt.allocPrint(
                        result.errors.allocator,
                        "Must be at least {d}",
                        .{min},
                    );
                    try addError(result, field_name, validation, msg);
                    is_valid = false;
                }
            }

            if (validation.max_value) |max| {
                if (value > max) {
                    const msg = try std.fmt.allocPrint(
                        result.errors.allocator,
                        "Must be at most {d}",
                        .{max},
                    );
                    try addError(result, field_name, validation, msg);
                    is_valid = false;
                }
            }
        },

        else => {},
    }

    // Custom validation
    if (validation.custom) |custom_fn| {
        if (!custom_fn(value)) {
            try addError(result, field_name, validation, "Validation failed");
            is_valid = false;
        }
    }

    return is_valid;
}

// Validation helper functions
fn validateEmail(email: []const u8) bool {
    if (email.len < 5) return false;

    // Check for @ symbol
    const at_index = std.mem.indexOf(u8, email, "@") orelse return false;
    if (at_index == 0 or at_index == email.len - 1) return false;

    // Check for multiple @ symbols
    const after_at = email[at_index + 1 ..];
    if (std.mem.indexOf(u8, after_at, "@") != null) return false;

    // Check for domain dot
    const dot_index = std.mem.lastIndexOf(u8, after_at, ".") orelse return false;
    if (dot_index == 0 or dot_index == after_at.len - 1) return false;

    // Basic character validation
    for (email) |char| {
        if (!std.ascii.isAlphanumeric(char) and
            char != '@' and char != '.' and
            char != '-' and char != '_' and
            char != '+')
        {
            return false;
        }
    }

    return true;
}

fn validatePassword(password: []const u8) bool {
    if (password.len < 8) return false;

    var has_upper = false;
    var has_lower = false;
    var has_digit = false;

    for (password) |char| {
        if (std.ascii.isUpper(char)) has_upper = true;
        if (std.ascii.isLower(char)) has_lower = true;
        if (std.ascii.isDigit(char)) has_digit = true;
    }

    return has_upper and has_lower and has_digit;
}

fn validateUrl(url: []const u8) bool {
    // Basic URL validation
    if (url.len < 10) return false;

    // Check for protocol
    const has_http = std.mem.startsWith(u8, url, "http://");
    const has_https = std.mem.startsWith(u8, url, "https://");

    return has_http or has_https;
}

fn validatePhone(phone: []const u8) bool {
    // Basic phone validation (digits, spaces, hyphens, parentheses, plus)
    if (phone.len < 10) return false;

    var digit_count: usize = 0;
    for (phone) |char| {
        if (std.ascii.isDigit(char)) {
            digit_count += 1;
        } else if (char != ' ' and char != '-' and char != '(' and char != ')' and char != '+') {
            return false;
        }
    }

    return digit_count >= 10 and digit_count <= 15;
}

fn validateAlphanumeric(str: []const u8) bool {
    for (str) |char| {
        if (!std.ascii.isAlphanumeric(char)) return false;
    }
    return true;
}

// fn validation(form_value: anytype, evt: *Vapor.Event) void {
//     Vapor.print("VALIDATION\n", .{});
//     evt.preventDefault();
//     const validations = @TypeOf(form_value).__validations;
//
//     // const form_fields = @typeInfo(@TypeOf(form_value)).@"struct".fields;
//
//     // Parse form data
//     const parsed_form: @TypeOf(form_value) = evt.formData(&form_value) orelse return;
//
//     // These are the Validation fields
//     const validation_fields = @typeInfo(Validation).@"struct".fields;
//
//     // Get form validations fields, these are fields that are specified in form __validations anonymous struct
//     const form_validation_fields = @typeInfo(@TypeOf(validations)).@"struct".fields;
//
//     // Loop through the __validations fields these are the form fields, like username, password ect
//     inline for (form_validation_fields) |field| {
//         // This is the field name for the form, and validation, they must be matching
//         const field_name = field.name;
//         // Get the value from the form
//         const field_value = @field(parsed_form, field_name);
//
//         // Get the validation value Validation, like .{ .min = 3, .max = 10, .err = "err msg" }
//         const form_validation_value = field.defaultValue() orelse {
//             Vapor.print("No validation value\n", .{});
//         };
//
//         // loop through the validation fields
//         // And check if the field value of parsed form matches the validation value
//         inline for (validation_fields) |validation_field| {
//             const validation_name = validation_field.name;
//             const validation_type = validation_field.type;
//             const validation_value = @field(form_validation_value, validation_field.name);
//             if (validation_value == null) continue;
//             if (!std.mem.eql(u8, "err", validation_name)) {
//                 switch (@TypeOf(field_value)) {
//                     usize, u32, u64, i32, i64, f32, f64 => {
//                         if (std.mem.eql(u8, "max", validation_name)) {
//                             if (field_value < validation_value.?) {
//                                 Vapor.print("Max validation failed\n", .{});
//                             }
//                         }
//                         if (std.mem.eql(u8, "min", validation_name)) {
//                             if (field_value > validation_value.?) {
//                                 Vapor.print("Max validation failed\n", .{});
//                             }
//                         }
//                     },
//                     []const u8 => {
//                         switch (@typeInfo(Vapor.Kit.getUnderlyingType(validation_type))) {
//                             .pointer => {
//                                 if (std.mem.eql(u8, "includes", validation_name)) {
//                                     if (std.mem.indexOf(u8, field_value, validation_value.?) == null) {
//                                         Vapor.printErr("Includes validation failed\n", .{});
//                                     }
//                                 }
//                             },
//                             .int => {
//                                 if (std.mem.eql(u8, "min", validation_name)) {
//                                     if (field_value.len < validation_value.?) {
//                                         Vapor.printErr("Validation failed for min\n", .{});
//                                     }
//                                 }
//                                 if (std.mem.eql(u8, "max", validation_name)) {
//                                     if (field_value.len > validation_value.?) {
//                                         Vapor.printErr("Validation failed for max\n", .{});
//                                     }
//                                 }
//                             },
//                             .@"enum" => {
//                                 switch (validation_value.?) {
//                                     .email => {
//                                         const email_value = field_value;
//                                         if (std.mem.indexOf(u8, email_value, "@") == null) {
//                                             Vapor.printErr("Password requires an '@' symbol\n", .{});
//                                         }
//                                         if (email_value.len < 5) {
//                                             Vapor.printErr("Email requires a minimum of 5 characters\n", .{});
//                                         }
//                                     },
//                                     .password => {
//                                         if (field_value.len < 5) {
//                                             Vapor.printErr("Password requires a minimum of 5 characters\n", .{});
//                                         }
//                                     },
//                                     else => {},
//                                 }
//                             },
//                             else => {},
//                         }
//                     },
//                     else => {},
//                 }
//             }
//         }
//     }
// }

var field_index: usize = 0;
fn generateForm(self: *Self, parent: *Node, field_name: []const u8, comptime VT: type, value: anytype, comptime form_type: type, form_value: anytype) !void {
    current_parent = current_parent orelse parent;
    const validations = @TypeOf(form_type.__validations);
    switch (@typeInfo(VT)) {
        .@"struct" => {
            const struct_node = try self.createNode(.Box);
            self.attachChild(current_parent.?, struct_node);
            current_parent = struct_node;
            const fields = @typeInfo(VT).@"struct".fields;
            inline for (fields) |field| {
                const FT = field.type;
                var field_value: getUnderlyingType(FT) = undefined;
                field_value = field.defaultValue() orelse {
                    try self.generateForm(current_parent.?, field.name, field.type, {}, form_type, form_value);
                    continue;
                };
                try self.generateForm(current_parent.?, field.name, field.type, field_value, form_type, form_value);
                field_index += 1;
            }
            current_parent = parent;
        },
        .pointer => |ptr| {
            // switch (VT) {
            //     []const u8 => {
            //         Vapor.print("[]const u8 slice\n", .{});
            //         const node = try self.createNode(.Text);
            //         node.data = .{ .text = .{ .content = value } };
            //         self.attachChild(current_parent.?, node);
            //     },
            //     else => {},
            // }
            switch (ptr.size) {
                .c => {
                    Vapor.print("c\n", .{});
                },
                .many => {
                    Vapor.print("many\n", .{});
                },
                .one => {
                    Vapor.print("one\n", .{});
                    switch (ptr.child) {
                        fn () void => {
                            const node = try self.createNode(.Button);
                            node.data = .{ .button = .{ .text = field_name, .on_click = value } };
                            current_parent = node;
                            try self.generateForm(current_parent.?, field_name, []const u8, field_name, form_type, form_value);
                            current_parent = parent;
                            self.attachChild(current_parent.?, node);
                        },
                        else => {
                            const node = try self.createNode(.TextField);
                            node.data = .{ .textfield = .{
                                .title = field_name,
                            } };
                            self.attachChild(current_parent.?, node);
                        },
                    }
                },
                .slice => {
                    // Here we switch on the validation value
                    var _type: Vapor.Types.InputTypes = .string;
                    const fields = @typeInfo(validations).@"struct".fields; // this is like username password ect
                    inline for (fields) |field| blk: {
                        if (std.mem.eql(u8, field_name, field.name)) { // this checks that we are looping through the right field ie username
                            const field_validation = field.defaultValue() orelse continue; // This is the Validation struct
                            const field_validation_type = @field(field_validation, "field_type");
                            _type = field_validation_type.?;
                            break :blk;
                        }
                    }
                    const node = try self.createNode(.TextField);
                    node.data = .{ .textfield = .{
                        .title = field_name,
                        .type = _type,
                        .err = form_value.validation_error,
                        .err_index = field_index,
                    } };
                    self.attachChild(current_parent.?, node);
                },
            }
        },
        .int => {
            const node = try self.createNode(.TextField);
            node.data = .{ .textfield = .{
                .title = field_name,
                .type = .int,
                .err = form_value.validation_error,
                .err_index = field_index,
            } };
            self.attachChild(current_parent.?, node);
        },
        .@"enum" => {
            Vapor.print("enum\n", .{});
            // const node = try self.createNode(.TextField);
            // node.data = .{ .textfield = .{ .title = field_name } };
            // self.attachChild(current_parent.?, node);
        },
        // .float => {
        //     const node = try self.createNode(.Text);
        //     node.data = .{ .text = .{ .content = std.fmt.allocPrint(self.allocator, "{d}", .{value}) catch unreachable } };
        //     self.attachChild(current_parent.?, node);
        // },
        // .array => {
        //     const node = try self.createNode(.List);
        //     node.data = .{ .list = .{ .list_type = .disc } };
        //     self.attachChild(current_parent.?, node);
        //     current_parent = node;
        //     for (value) |item| {
        //         const list_item_node = try self.createNode(.ListItem);
        //         current_parent = list_item_node;
        //         try self.generate(current_parent.?, field_name, item);
        //         self.attachChild(node, list_item_node);
        //     }
        //     current_parent = parent;
        // },
        else => {},
    }
}

pub inline fn traverseForm(node: ?*Node, style: Style, comptime T: type, functions: ?[]const TaggedFunction(T), value: anytype) !void {
    var current = node;
    for (value.validation_error.?.errors.items) |err| {
        Vapor.printErr("{s}: {s}\n", .{ err.field, err.message });
    }
    while (current) |n| {
        switch (n.tag) {
            .Form => {
                Form(validateForm, value).style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                    .direction = .column,
                    .child_gap = 32,
                })({
                    try traverseFormValue(n.child, style, T, functions, value);

                    const button_style: Vapor.Types.Style = style.button_style;
                    SubmitButton().style(&button_style)({
                        Text("Submit").style(&.{
                            .visual = .{ .font_size = 16 },
                        });
                    });
                });
            },
            else => {},
        }
        current = n.next;
    }
}

pub fn traverseFormValue(node: ?*Node, style: Style, comptime T: type, functions: ?[]const TaggedFunction(T), value: anytype) !void {
    var current = node;

    const code_style: Vapor.Types.Style = style.code_style;
    const text_style: Vapor.Types.Style = style.text_style;
    const heading_style: Vapor.Types.Style = style.heading_style;
    const button_style: Vapor.Types.Style = style.button_style;
    const list_style: Vapor.Types.Style = style.list_style;
    const struct_style: Vapor.Types.Style = style.struct_style;

    while (current) |n| {
        switch (n.tag) {
            .Root => {
                try traverse(n.child, style, T, functions);
            },
            .Svg => |_| {
                const src = n.data.image.src;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                })({
                    Graphic(.{ .src = src }).style(&.{
                        .size = .{ .width = .percent(70), .height = .auto },
                        .visual = .{ .fill = style.text_style.visual.?.text_color, .stroke = style.text_style.visual.?.text_color },
                        .aspect_ratio = .landscape,
                    });
                });
            },
            .Box => {
                Box().style(&struct_style)({
                    try traverseFormValue(n.child, style, T, functions, value);
                });
            },
            .Button => {
                Button(.{ .on_press = n.data.button.on_click }).style(&button_style)({
                    try traverseFormValue(n.child, style, T, functions, value);
                });
            },

            .Image => |_| {
                const src = n.data.image.src;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                })({
                    Image(.{ .src = src }).style(&.{
                        .size = .{ .width = .percent(70), .height = .auto },
                        .aspect_ratio = .landscape,
                    });
                });
            },
            .Snippet => |_| {
                const snippet = n.data.snippet.ptr;
                const content = n.data.snippet.content;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .fit },
                    .margin = .tb(4, 4),
                    .layout = .center,
                })({
                    snippet.code(content);
                });
            },
            .Heading => |_| {
                Box().style(&.{
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(12, 12),
                })({
                    try traverseFormValue(n.child, .{
                        .is_bold = style.is_bold,
                        .is_italic = style.is_italic,
                        .level = n.data.heading.level,
                    }, T, functions, value);
                });
            },
            .Paragraph => {
                Box().style(&.{
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(6, 6),
                })({
                    try traverseFormValue(n.child, style, T, functions, value);
                });
            },

            .List => |_| {
                List().style(&list_style)({
                    try traverseFormValue(n.child, style, T, functions, value);
                });
            },
            .ListItem => |_| {
                ListItem().style(&.{})({
                    try traverseFormValue(n.child, style, T, functions, value);
                });
            },

            .Text => |_| {
                if (style.level > 0) {
                    switch (style.level) {
                        1 => Heading(style.level, n.data.text.content).style(&heading_style),
                        2 => Heading(style.level, n.data.text.content).style(&heading_style),
                        3 => Heading(style.level, n.data.text.content).style(&heading_style),
                        4 => Heading(style.level, n.data.text.content).style(&heading_style),
                        5 => Heading(style.level, n.data.text.content).style(&heading_style),
                        6 => Heading(style.level, n.data.text.content).style(&heading_style),
                        else => {},
                    }
                } else {
                    Text(n.data.text.content).layout(.in_line).font(16, if (style.is_bold) 900 else 400, text_style.visual.?.text_color).style(&text_style);
                }
            },
            .Bold => {
                try traverseFormValue(n.child, .{
                    .is_bold = true,
                    .is_italic = style.is_italic,
                    .level = style.level,
                }, T, functions, value);
            },
            .Section => {
                Section().id(n.data.section.id).style(&.{
                    .child_gap = 4,
                    .direction = .column,
                    .size = .hw(.percent(100), .grow),
                    .layout = .{},
                })({
                    try traverseFormValue(n.child, .{
                        .is_bold = style.is_bold,
                        .is_italic = style.is_italic,
                        .text_style = style.text_style,
                        .heading_style = style.heading_style,
                        .level = style.level,
                    }, T, functions, value);
                });
            },
            .Italic => {
                try traverseFormValue(n.child, .{
                    .is_bold = style.is_bold,
                    .is_italic = true,
                    .level = style.level,
                }, T, functions, value);
            },
            .CodeBlock => |_| {
                var editor = n.data.code_block.editor;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .fit },
                    .margin = .tb(16, 32),
                    .layout = .center,
                })({
                    editor.render(0);
                });
            },
            .RunBlock => |_| {
                const src = n.data.run_block.src;
                const fncs = functions orelse return;
                for (fncs) |func| {
                    if (std.mem.eql(u8, src, func.tag)) {
                        if (func.args) |args| {
                            @call(.auto, func.function.?, .{args});
                        } else {
                            @call(.auto, func.no_args_function.?, .{});
                        }
                    }
                }
            },
            .Link => |_| {
                const href = n.data.link.href;
                const title = n.data.link.title;
                Link(.{ .url = href, .aria_label = title }).style(&.{
                    .layout = .in_line,
                    .visual = .{
                        .cursor = .pointer,
                    },
                })({
                    Text(n.data.link.href).style(&.{
                        .layout = .in_line,
                        .visual = .{
                            .font_size = 16,
                            .font_weight = if (style.is_bold) 900 else 400,
                            .font_style = if (style.is_italic) .italic else null,
                        },
                    });
                });
            },
            .Code => |_| {
                Code(n.data.code.content).style(&code_style);
            },
            .TextField => {
                Box().layout(.left_center).direction(.column).width(.percent(100)).height(.fit).children({
                    Label(n.data.textfield.title).style(&.{
                        .layout = .in_line,
                        .visual = .{
                            .font_size = 16,
                        },
                    });
                    TextField(n.data.textfield.type).fieldName(n.data.textfield.title).style(&.{
                        .size = .hw(.px(38), .percent(100)),
                        .padding = .tblr(4, 4, 8, 8),
                        .font_family = "IBM Plex Mono,monospace",
                        .outline = .none,
                        .interactive = .{
                            .hover = .{
                                .border = .simple(.palette(.border_color_light)),
                            },
                        },
                        .visual = .{
                            .border = .simple(.hex("#E1E1E1")),
                            .background = .palette(.background),
                        },
                    });
                    if (n.data.textfield.err) |err| {
                        if (err.errors.items.len > n.data.textfield.err_index) {
                            Text(err.errors.items[n.data.textfield.err_index].message)
                                .font(12, null, .red)
                                .height(.px(16))
                                .width(.percent(100))
                                .ellipsis(.dot)
                                .end();
                        } else {
                            Text("error")
                                .font(12, null, .transparent)
                                .height(.px(16))
                                .width(.percent(100))
                                .ellipsis(.dot)
                                .end();
                        }
                    }
                });
            },
            else => {},
        }
        current = n.next;
    }
}

fn generate(self: *Self, parent: *Node, field_name: []const u8, value: anytype) !void {
    const VT = @TypeOf(value);
    Vapor.print("value: {any}\n", .{VT});
    current_parent = current_parent orelse parent;
    switch (@typeInfo(VT)) {
        .@"struct" => {
            const struct_node = try self.createNode(.Box);
            self.attachChild(current_parent.?, struct_node);
            current_parent = struct_node;
            const fields = @typeInfo(VT).@"struct".fields;
            inline for (fields) |field| {
                const FT = field.type;
                var field_value: getUnderlyingType(FT) = undefined;
                if (@typeInfo(FT) == .optional) {
                    field_value = @field(value, field.name) orelse continue;
                } else {
                    field_value = @field(value, field.name);
                }
                try self.generate(current_parent.?, field.name, field_value);
            }
            current_parent = parent;
        },
        .pointer => |ptr| {
            // switch (VT) {
            //     []const u8 => {
            //         Vapor.print("[]const u8 slice\n", .{});
            //         const node = try self.createNode(.Text);
            //         node.data = .{ .text = .{ .content = value } };
            //         self.attachChild(current_parent.?, node);
            //     },
            //     else => {},
            // }
            switch (ptr.size) {
                .c => {
                    Vapor.print("c\n", .{});
                },
                .many => {
                    Vapor.print("many\n", .{});
                },
                .one => {
                    Vapor.print("one\n", .{});
                    switch (ptr.child) {
                        fn () void => {
                            const node = try self.createNode(.Button);
                            node.data = .{ .button = .{ .text = field_name, .on_click = value } };
                            current_parent = node;
                            try self.generate(current_parent.?, field_name, field_name);
                            current_parent = parent;
                            self.attachChild(current_parent.?, node);
                        },
                        else => {
                            Vapor.print("slice\n", .{});
                            const node = try self.createNode(.Text);
                            node.data = .{ .text = .{ .content = value } };
                            self.attachChild(current_parent.?, node);
                        },
                    }
                },
                .slice => {
                    Vapor.print("slice\n", .{});
                    const node = try self.createNode(.Text);
                    node.data = .{ .text = .{ .content = value } };
                    self.attachChild(current_parent.?, node);
                },
            }
        },
        .int => {
            const node = try self.createNode(.Text);
            node.data = .{ .text = .{ .content = std.fmt.allocPrint(self.allocator, "{d}", .{value}) catch unreachable } };
            self.attachChild(current_parent.?, node);
        },
        .float => {
            const node = try self.createNode(.Text);
            node.data = .{ .text = .{ .content = std.fmt.allocPrint(self.allocator, "{d}", .{value}) catch unreachable } };
            self.attachChild(current_parent.?, node);
        },
        .array => {
            const node = try self.createNode(.List);
            node.data = .{ .list = .{ .list_type = .disc } };
            self.attachChild(current_parent.?, node);
            current_parent = node;
            for (value) |item| {
                const list_item_node = try self.createNode(.ListItem);
                current_parent = list_item_node;
                try self.generate(current_parent.?, field_name, item);
                self.attachChild(node, list_item_node);
            }
            current_parent = parent;
        },
        else => {},
    }
}

pub fn traverse(node: ?*Node, style: Style, comptime T: type, functions: ?[]const TaggedFunction(T)) !void {
    var current = node;
    while (current) |n| {
        switch (n.tag) {
            .Root => {
                try traverse(n.child, style, T, functions);
            },
            .Svg => |_| {
                const src = n.data.image.src;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                })({
                    Graphic(.{ .src = src }).style(&.{
                        .size = .{ .width = .percent(70), .height = .auto },
                        .visual = .{ .fill = style.text_color, .stroke = style.text_color },
                        .aspect_ratio = .landscape,
                    });
                });
            },
            .Image => |_| {
                const src = n.data.image.src;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(32, 32),
                    .layout = .center,
                })({
                    Image(.{ .src = src }).style(&.{
                        .size = .{ .width = .percent(70), .height = .auto },
                        .aspect_ratio = .landscape,
                    });
                });
            },
            .Snippet => |_| {
                const snippet = n.data.snippet.ptr;
                const content = n.data.snippet.content;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .fit },
                    .margin = .tb(4, 4),
                    .layout = .center,
                })({
                    snippet.code(content);
                });
            },
            .Heading => |_| {
                Box().style(&.{
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(12, 12),
                })({
                    try traverse(n.child, .{
                        .is_bold = style.is_bold,
                        .is_italic = style.is_italic,
                        .level = n.data.heading.level,
                    }, T, functions);
                });
            },
            .Paragraph => {
                Box().style(&.{
                    .layout = .in_line,
                    .size = .w(.percent(100)),
                    .padding = .tb(6, 6),
                })({
                    try traverse(n.child, style, T, functions);
                });
            },

            .List => |_| {
                List().style(&.{
                    .layout = .left_center,
                    .direction = .column,
                    .size = .w(.percent(100)),
                    .child_gap = 8,
                    .list_style = n.data.list.list_type,
                })({
                    try traverse(n.child, style, T, functions);
                });
            },
            .ListItem => |_| {
                ListItem().children({
                    try traverse(n.child, style, T, functions);
                });
            },

            .Text => |_| {
                if (style.level > 0) {
                    switch (style.level) {
                        1 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 500 },
                            .font_family = "IBM Plex Sans,sans-serif",
                            .margin = .tb(16, 0),
                        }),
                        2 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 400, .font_size = 28 },
                            .font_family = "IBM Plex Sans,sans-serif",
                            .margin = .tb(16, 0),
                        }),
                        3 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 600, .font_size = 24 },
                            .font_family = "IBM Plex Sans,sans-serif",
                            .margin = .tb(12, 0),
                        }),
                        4 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 400, .font_size = 20 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        5 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 300, .font_size = 18 },
                            .font_family = "IBM Plex Sans,sans-serif",
                        }),
                        6 => Heading(style.level, n.data.text.content).style(&.{
                            .visual = .{ .font_weight = 200 },
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
                            .font_style = if (style.is_italic) .italic else null,
                        },
                    });
                }
            },
            .Bold => {
                Vapor.print("bold {s}\n", .{n.child.?.data.text.content});
                try traverse(n.child, .{
                    .is_bold = true,
                    .is_italic = style.is_italic,
                    .level = style.level,
                }, T, functions);
            },
            .Section => {
                Section().id(n.data.section.id).style(&.{
                    .child_gap = 4,
                    .direction = .column,
                    .size = .hw(.percent(100), .grow),
                    .layout = .{},
                })({
                    try traverse(n.child, .{
                        .is_bold = style.is_bold,
                        .is_italic = style.is_italic,
                        .code_color = style.code_color,
                        .text_color = style.text_color,
                        .heading_color = style.heading_color,
                        .level = style.level,
                    }, T, functions);
                });
            },
            .Italic => {
                try traverse(n.child, .{
                    .is_bold = style.is_bold,
                    .is_italic = true,
                    .level = style.level,
                }, T, functions);
            },
            .CodeBlock => |_| {
                var editor = n.data.code_block.editor;
                Box().style(&.{
                    .size = .{ .width = .percent(100), .height = .fit },
                    .margin = .tb(16, 32),
                    .layout = .center,
                })({
                    editor.render(0);
                });
            },
            .RunBlock => |_| {
                const src = n.data.run_block.src;
                const fncs = functions orelse return;
                for (fncs) |func| {
                    if (std.mem.eql(u8, src, func.tag)) {
                        if (func.args) |args| {
                            @call(.auto, func.function.?, .{args});
                        } else {
                            @call(.auto, func.no_args_function.?, .{});
                        }
                    }
                }
            },
            .Link => |_| {
                const href = n.data.link.href;
                const title = n.data.link.title;
                Link(.{ .url = href, .aria_label = title }).style(&.{
                    .layout = .in_line,
                    .visual = .{
                        .cursor = .pointer,
                    },
                })({
                    Text(n.data.link.href).style(&.{
                        .layout = .in_line,
                        .visual = .{
                            .font_size = 16,
                            .font_weight = if (style.is_bold) 900 else 400,
                            .text_color = style.code_color,
                            .font_style = if (style.is_italic) .italic else null,
                        },
                    });
                });
            },
            .Code => |_| {
                Code(n.data.code.content).style(&.{
                    .layout = .in_line,
                    .visual = .{
                        .font_size = 15,
                        .font_weight = if (style.is_bold) 900 else 400,
                        .text_color = style.code_color,
                    },
                    .font_family = "JetBrains Mono,Fira Code,Consolas,monospace",
                });
            },
            else => {},
        }
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
        \\# Main Heading
        \\
        \\- Item 1
        \\  - Nested item 1
        \\  - Nested item 2
        \\- Item 2
        \\  - Nested item 3
        \\
        \\This is the second paragraph.
    ;
    // --------------------------------------------------   // ---------------------------------

    // 1. Initialize the parser
    var parser = Parser.init(allocator, markdown);
    var vaporize = init(allocator);
    vaporize.compile("Hello World!");

    // 2. Run the parser
    const root_node = try parser.parse();

    // 3. Traverse and print the tree
    try stdout.print("--- AST Traversal ---\n", .{});
    try traversePrint(root_node, stdout, 0);
    stdout.flush() catch {};
}

