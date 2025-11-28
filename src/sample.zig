
/// ## The Parser
///
/// This struct holds the state of the parsing process.
/// It uses an ArenaAllocator for all Node allocations.
pub const Parser = struct {
    arena: Allocator, // The parent allocator (e.g., GPA)
    source: []const u8,
    pos: usize,
    stack: ?*Item = null,

    /// Initializes a new parser with a given allocator and source text.
    pub fn init(allocator: Allocator, source: []const u8) Parser {
        const node = allocator.create(Node) catch unreachable;
        node.* = .{
            .tag = .Root,
            .data = .{ .none = {} }, // Default to none
            .child = null,
            .next = null,
        };
        const item = allocator.create(Item) catch unreachable;
        item.* = .{
            .ptr = node,
            .next = null,
        };
        return Parser{
            .arena = allocator, // Get the allocator interface
            .source = source,
            .pos = 0,
            .stack = item,
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
        try self.stackRegister(node);
        return node;
    }

    pub fn stackRegister(parser: *Parser, node: *Node) !void {
        const item: *Item = try parser.arena.create(Item);
        item.* = .{ .ptr = node };

        const current_stack = parser.stack;
        if (current_stack) |stack| {
            item.next = stack;
        }

        parser.stack = item;
    }

    pub fn stackPop(parser: *Parser) void {
        const current_stack = parser.stack orelse return;
        parser.stack = current_stack.next;
    }

    /// --- Parsing Logic ---
    /// The main parsing function. Returns the Root node of the AST.
    pub fn parse(parser: *Parser) !*Node {
        const stack = parser.stack orelse return error.StackEmpty;
        const root = stack.ptr orelse return error.StackEmpty;
        var last_section: ?*Node = null;

        while (!parser.isAtEnd()) {
            parser.skipBlankLines();
            if (parser.isAtEnd()) break;

            const block = try parser.parseBlock();

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
                    parser.attachChild(parent, block);
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


