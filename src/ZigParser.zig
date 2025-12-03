const std = @import("std");
const Allocator = std.mem.Allocator;
const Vapor = @import("vapor");

const Box = Vapor.Box;
const Text = Vapor.Text;
const CtxButton = Vapor.CtxButton;
const Icon = Vapor.Icon;
const Graphic = Vapor.Graphic;
const List = Vapor.List;
const ListItem = Vapor.ListItem;

const svg_bug =
    \\<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="white" class="bi bi-bug-fill" viewBox="0 0 16 16">
    \\  <path d="M4.978.855a.5.5 0 1 0-.956.29l.41 1.352A5 5 0 0 0 3 6h10a5 5 0 0 0-1.432-3.503l.41-1.352a.5.5 0 1 0-.956-.29l-.291.956A5 5 0 0 0 8 1a5 5 0 0 0-2.731.811l-.29-.956z"/>
    \\  <path d="M13 6v1H8.5v8.975A5 5 0 0 0 13 11h.5a.5.5 0 0 1 .5.5v.5a.5.5 0 1 0 1 0v-.5a1.5 1.5 0 0 0-1.5-1.5H13V9h1.5a.5.5 0 0 0 0-1H13V7h.5A1.5 1.5 0 0 0 15 5.5V5a.5.5 0 0 0-1 0v.5a.5.5 0 0 1-.5.5zm-5.5 9.975V7H3V6h-.5a.5.5 0 0 1-.5-.5V5a.5.5 0 0 0-1 0v.5A1.5 1.5 0 0 0 2.5 7H3v1H1.5a.5.5 0 0 0 0 1H3v1h-.5A1.5 1.5 0 0 0 1 11.5v.5a.5.5 0 1 0 1 0v-.5a.5.5 0 0 1 .5-.5H3a5 5 0 0 0 4.5 4.975"/>
    \\</svg>
;

// ============================================================================
// Color Type (compatible with Vapor's color system)
// ============================================================================

pub const Color = union(enum) {
    hex: []const u8,
    palette: PaletteColor,
    transparent,

    pub const PaletteColor = enum {
        code_text_color,
        code_keyword_color,
        code_function_color,
        code_string_color,
        code_number_color,
        code_comment_color,
        code_type_color,
        code_builtin_color,
        code_operator_color,
        code_tint_color,
    };

    pub fn eql(self: Color, other: Color) bool {
        return switch (self) {
            .hex => |h| switch (other) {
                .hex => |oh| std.mem.eql(u8, h, oh),
                else => false,
            },
            .palette => |p| switch (other) {
                .palette => |op| p == op,
                else => false,
            },
            .transparent => other == .transparent,
        };
    }
};

// ============================================================================
// Token Types
// ============================================================================

pub const TokenType = enum {
    // Keywords
    keyword,
    // Types
    type_name,
    // Built-in functions (@import, @ptrCast, etc.)
    builtin,
    // Function names (in declarations and calls)
    function,

    // Component type
    component,

    // String literals
    string,
    // Character literals
    char,
    // Number literals
    number,
    // Comments
    comment,
    // Operators and punctuation
    operator,
    // Identifiers (variables, field names)
    identifier,
    // Whitespace
    whitespace,
    // Newline (for line tracking)
    newline,
};

pub const Token = struct {
    type: TokenType,
    text: []const u8,
    color: Color,
    line: usize,
    column: usize,
};

// ============================================================================
// AST Node Types
// ============================================================================

pub const NodeType = enum {
    root,
    line,
    token,
    error_annotation,
};

pub const ErrorType = enum {
    unterminated_string,
    unexpected_token,
    invalid_syntax,
    // ... more error types
};

pub const Node = struct {
    tag: NodeType,
    data: union {
        root: void,
        line: struct {
            number: usize,
            indent: usize,
            is_added: bool,
            is_removed: bool,
        },
        token: Token,
        error_annotation: struct { // NEW
            error_type: ErrorType,
            message: []const u8,
            start_pos: usize,
            end_pos: usize,
            line: usize,
            column: usize,
        },
    },
    child: ?*Node,
    next: ?*Node,
};

// ============================================================================
// Zig Keywords and Types
// ============================================================================

const keywords = [_][]const u8{
    "addrspace",   "align",       "allowzero", "and",         "anyframe",
    "anytype",     "asm",         "async",     "await",       "break",
    "callconv",    "catch",       "comptime",  "const",       "continue",
    "defer",       "else",        "enum",      "errdefer",    "error",
    "export",      "extern",      "fn",        "for",         "if",
    "inline",      "linksection", "noalias",   "nosuspend",   "opaque",
    "or",          "orelse",      "packed",    "pub",         "resume",
    "return",      "struct",      "suspend",   "switch",      "test",
    "threadlocal", "try",         "union",     "unreachable", "usingnamespace",
    "var",         "volatile",    "while",
};

const primitive_types = [_][]const u8{
    "i8",        "i16",    "i32",       "i64",        "i128",         "isize",
    "u8",        "u16",    "u32",       "u64",        "u128",         "usize",
    "f16",       "f32",    "f64",       "f80",        "f128",         "bool",
    "void",      "null",   "undefined", "anyerror",   "comptime_int", "comptime_float",
    "anyopaque", "type",   "noreturn",  "c_short",    "c_ushort",     "c_int",
    "c_uint",    "c_long", "c_ulong",   "c_longlong", "c_ulonglong",  "c_longdouble",
    "c_void",
};

// ============================================================================
// Tokenizer
// ============================================================================

pub const Tokenizer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn next(self: *Tokenizer) ?Token {
        if (self.isAtEnd()) return null;

        const start_pos = self.pos;
        const start_line = self.line;
        const start_col = self.column;

        const char = self.peek();

        // Newline
        if (char == '\n') {
            _ = self.advance();
            self.line += 1;
            self.column = 1;
            return .{
                .type = .newline,
                .text = self.source[start_pos..self.pos],
                .color = .transparent,
                .line = start_line,
                .column = start_col,
            };
        }

        // Whitespace (excluding newline)
        if (isWhitespace(char) and char != '\n') {
            while (!self.isAtEnd() and isWhitespace(self.peek()) and self.peek() != '\n') {
                _ = self.advance();
            }
            return .{
                .type = .whitespace,
                .text = self.source[start_pos..self.pos],
                .color = .transparent,
                .line = start_line,
                .column = start_col,
            };
        }

        // Comments
        if (char == '/' and self.peekNext() == '/') {
            while (!self.isAtEnd() and self.peek() != '\n') {
                _ = self.advance();
            }
            return .{
                .type = .comment,
                .text = self.source[start_pos..self.pos],
                .color = .{ .palette = .code_comment_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // String literals
        if (char == '"') {
            return self.tokenizeString(start_pos, start_line, start_col);
        }

        // Character literals
        if (char == '\'') {
            return self.tokenizeChar(start_pos, start_line, start_col);
        }

        // Multi-line string literals
        if (char == '\\' and self.peekNext() == '\\') {
            while (!self.isAtEnd() and self.peek() != '\n') {
                _ = self.advance();
            }
            return .{
                .type = .string,
                .text = self.source[start_pos..self.pos],
                .color = .{ .palette = .code_string_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // Numbers
        if (std.ascii.isDigit(char)) {
            return self.tokenizeNumber(start_pos, start_line, start_col);
        }

        // Builtin functions (@identifier)
        if (char == '@') {
            _ = self.advance();
            while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
                _ = self.advance();
            }
            return .{
                .type = .builtin,
                .text = self.source[start_pos..self.pos],
                .color = .{ .palette = .code_builtin_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // Identifiers and keywords
        if (std.ascii.isAlphabetic(char) or char == '_') {
            return self.tokenizeIdentifier(start_pos, start_line, start_col);
        }

        // Operators and punctuation
        return self.tokenizeOperator(start_pos, start_line, start_col);
    }

    fn tokenizeString(self: *Tokenizer, start_pos: usize, start_line: usize, start_col: usize) Token {
        _ = self.advance(); // consume opening quote
        while (!self.isAtEnd()) {
            const c = self.peek();
            if (c == '"') {
                _ = self.advance();
                break;
            }
            if (c == '\\' and !self.isAtEnd()) {
                _ = self.advance(); // skip escape char
            }
            if (c == '\n') break; // unterminated
            _ = self.advance();
        }
        return .{
            .type = .string,
            .text = self.source[start_pos..self.pos],
            .color = .{ .palette = .code_string_color },
            .line = start_line,
            .column = start_col,
        };
    }

    fn tokenizeChar(self: *Tokenizer, start_pos: usize, start_line: usize, start_col: usize) Token {
        _ = self.advance(); // consume opening quote
        if (!self.isAtEnd() and self.peek() == '\\') {
            _ = self.advance();
            if (!self.isAtEnd()) _ = self.advance();
        } else if (!self.isAtEnd()) {
            _ = self.advance();
        }
        if (!self.isAtEnd() and self.peek() == '\'') {
            _ = self.advance();
        }
        return .{
            .type = .char,
            .text = self.source[start_pos..self.pos],
            .color = .{ .palette = .code_string_color },
            .line = start_line,
            .column = start_col,
        };
    }

    fn tokenizeNumber(self: *Tokenizer, start_pos: usize, start_line: usize, start_col: usize) Token {
        // Handle different number formats: 0x, 0o, 0b, decimal, float
        if (self.peek() == '0' and self.pos + 1 < self.source.len) {
            const next_c = self.source[self.pos + 1];
            if (next_c == 'x' or next_c == 'X') {
                _ = self.advance();
                _ = self.advance();
                while (!self.isAtEnd() and (std.ascii.isHex(self.peek()) or self.peek() == '_')) {
                    _ = self.advance();
                }
            } else if (next_c == 'o' or next_c == 'O') {
                _ = self.advance();
                _ = self.advance();
                while (!self.isAtEnd() and (self.peek() >= '0' and self.peek() <= '7' or self.peek() == '_')) {
                    _ = self.advance();
                }
            } else if (next_c == 'b' or next_c == 'B') {
                _ = self.advance();
                _ = self.advance();
                while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1' or self.peek() == '_')) {
                    _ = self.advance();
                }
            } else {
                self.consumeDecimal();
            }
        } else {
            self.consumeDecimal();
        }
        return .{
            .type = .number,
            .text = self.source[start_pos..self.pos],
            .color = .{ .palette = .code_number_color },
            .line = start_line,
            .column = start_col,
        };
    }

    fn consumeDecimal(self: *Tokenizer) void {
        while (!self.isAtEnd() and (std.ascii.isDigit(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }
        // Float part
        if (!self.isAtEnd() and self.peek() == '.' and self.pos + 1 < self.source.len and std.ascii.isDigit(self.source[self.pos + 1])) {
            _ = self.advance(); // consume '.'
            while (!self.isAtEnd() and (std.ascii.isDigit(self.peek()) or self.peek() == '_')) {
                _ = self.advance();
            }
        }
        // Exponent
        if (!self.isAtEnd() and (self.peek() == 'e' or self.peek() == 'E')) {
            _ = self.advance();
            if (!self.isAtEnd() and (self.peek() == '+' or self.peek() == '-')) {
                _ = self.advance();
            }
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
    }

    fn tokenizeIdentifier(self: *Tokenizer, start_pos: usize, start_line: usize, start_col: usize) Token {
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        const text = self.source[start_pos..self.pos];

        // Check if it's a keyword
        if (isKeyword(text)) {
            return .{
                .type = .keyword,
                .text = text,
                .color = .{ .palette = .code_keyword_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // Check if it's a primitive type
        if (isPrimitiveType(text)) {
            return .{
                .type = .type_name,
                .text = text,
                .color = .{ .palette = .code_type_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // Check if it's a function call (followed by '(')
        var lookahead = self.pos;
        while (lookahead < self.source.len and isWhitespace(self.source[lookahead])) {
            lookahead += 1;
        }

        if (std.ascii.isUpper(text[0]) and lookahead < self.source.len and self.source[lookahead] == '(') {
            return .{
                .type = .component,
                .text = text,
                .color = .{ .palette = .code_tint_color },
                .line = start_line,
                .column = start_col,
            };
        } else if (lookahead < self.source.len and self.source[lookahead] == '(') {
            return .{
                .type = .function,
                .text = text,
                .color = .{ .palette = .code_function_color },
                .line = start_line,
                .column = start_col,
            };
        }

        // Check if it looks like a type (PascalCase)
        if (text.len > 0 and std.ascii.isUpper(text[0])) {
            return .{
                .type = .type_name,
                .text = text,
                .color = .{ .palette = .code_type_color },
                .line = start_line,
                .column = start_col,
            };
        }

        return .{
            .type = .identifier,
            .text = text,
            .color = .{ .palette = .code_text_color },
            .line = start_line,
            .column = start_col,
        };
    }

    fn tokenizeOperator(self: *Tokenizer, start_pos: usize, start_line: usize, start_col: usize) Token {
        const char = self.advance();

        // Multi-character operators
        const next_c = if (!self.isAtEnd()) self.peek() else 0;

        switch (char) {
            '=' => if (next_c == '=' or next_c == '>') {
                _ = self.advance();
            },
            '!' => if (next_c == '=') {
                _ = self.advance();
            },
            '<' => if (next_c == '=' or next_c == '<') {
                _ = self.advance();
                if (!self.isAtEnd() and self.peek() == '=') _ = self.advance();
            },
            '>' => if (next_c == '=' or next_c == '>') {
                _ = self.advance();
                if (!self.isAtEnd() and self.peek() == '=') _ = self.advance();
            },
            '+' => if (next_c == '=' or next_c == '+') {
                _ = self.advance();
            },
            '-' => if (next_c == '=' or next_c == '-' or next_c == '>') {
                _ = self.advance();
            },
            '*' => if (next_c == '=' or next_c == '*') {
                _ = self.advance();
            },
            '/' => if (next_c == '=') {
                _ = self.advance();
            },
            '%' => if (next_c == '=') {
                _ = self.advance();
            },
            '&' => if (next_c == '=' or next_c == '&') {
                _ = self.advance();
            },
            '|' => if (next_c == '=' or next_c == '|') {
                _ = self.advance();
            },
            '^' => if (next_c == '=') {
                _ = self.advance();
            },
            '.' => if (next_c == '.' or next_c == '*') {
                _ = self.advance();
                if (!self.isAtEnd() and self.peek() == '.') _ = self.advance();
            },
            else => {},
        }

        return .{
            .type = .operator,
            .text = self.source[start_pos..self.pos],
            .color = .{ .palette = .code_operator_color },
            .line = start_line,
            .column = start_col,
        };
    }

    fn isAtEnd(self: *const Tokenizer) bool {
        return self.pos >= self.source.len;
    }

    fn peek(self: *const Tokenizer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *const Tokenizer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn advance(self: *Tokenizer) u8 {
        if (self.isAtEnd()) return 0;
        const char = self.source[self.pos];
        self.pos += 1;
        self.column += 1;
        return char;
    }
};

// ============================================================================
// Parser - Builds AST from tokens
// ============================================================================

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: Tokenizer,

    pub fn init(allocator: Allocator, source: []const u8) Parser {
        return .{
            .allocator = allocator,
            .tokenizer = Tokenizer.init(source),
        };
    }

    pub fn parse(self: *Parser) !*Node {
        const root = try self.createNode(.root);

        var current_line: ?*Node = null;
        var current_line_number: usize = 1;
        var last_token_in_line: ?*Node = null;

        // Create first line
        current_line = try self.createLineNode(current_line_number);
        root.child = current_line;

        while (self.tokenizer.next()) |token| {
            if (token.type == .newline) {
                // Start a new line
                const new_line = try self.createLineNode(current_line_number + 1);
                current_line_number += 1;

                if (current_line) |line| {
                    line.next = new_line;
                }
                current_line = new_line;
                last_token_in_line = null;
            } else {
                // Add token to current line
                const token_node = try self.createTokenNode(token);

                if (last_token_in_line) |last| {
                    last.next = token_node;
                } else if (current_line) |line| {
                    line.child = token_node;
                }
                last_token_in_line = token_node;
            }
        }

        return root;
    }

    fn createNode(self: *Parser, tag: NodeType) !*Node {
        const node = try self.allocator.create(Node);
        node.* = .{
            .tag = tag,
            .data = .{ .root = {} },
            .child = null,
            .next = null,
        };
        return node;
    }

    fn createLineNode(self: *Parser, line_number: usize) !*Node {
        const node = try self.allocator.create(Node);
        node.* = .{
            .tag = .line,
            .data = .{
                .line = .{
                    .number = line_number,
                    .indent = 0,
                    .is_added = false,
                    .is_removed = false,
                },
            },
            .child = null,
            .next = null,
        };
        return node;
    }

    fn createTokenNode(self: *Parser, token: Token) !*Node {
        const node = try self.allocator.create(Node);
        node.* = .{
            .tag = .token,
            .data = .{ .token = token },
            .child = null,
            .next = null,
        };
        return node;
    }
};

// ============================================================================
// Syntax Highlighter - High-level API
// ============================================================================

const ErrorAnnotation = struct {
    line: usize,
    column: usize,
    message: []const u8,
};

pub const SyntaxHighlighter = struct {
    allocator: Allocator,
    root: ?*Node,
    local_copy_code: []const u8 = undefined,
    show_cpy_btn: bool = false,
    use_cpy_btn: bool = true,
    errors: std.AutoHashMap(usize, ErrorAnnotation),

    pub fn init(allocator: Allocator) SyntaxHighlighter {
        return .{
            .allocator = allocator,
            .root = null,
            .errors = std.AutoHashMap(usize, ErrorAnnotation).init(allocator),
        };
    }

    pub fn parse(self: *SyntaxHighlighter, source: []const u8) !void {
        var parser = Parser.init(self.allocator, source);
        self.local_copy_code = source;
        self.root = try parser.parse();
    }

    /// Iterator for lines
    pub fn lines(self: *const SyntaxHighlighter) LineIterator {
        return .{
            .current = if (self.root) |r| r.child else null,
        };
    }

    pub const LineIterator = struct {
        current: ?*Node,

        pub fn next(self: *LineIterator) ?*Node {
            if (self.current) |node| {
                self.current = node.next;
                return node;
            }
            return null;
        }
    };

    /// Get tokens for a line
    pub fn tokensForLine(_: *const SyntaxHighlighter, line: *Node) TokenIterator {
        return .{
            .current = line.child,
        };
    }

    pub const TokenIterator = struct {
        current: ?*Node,

        pub fn next(self: *TokenIterator) ?Token {
            while (self.current) |node| {
                self.current = node.next;
                if (node.tag == .token) {
                    return node.data.token;
                }
            }
            return null;
        }
    };

    pub fn validateAST(self: *SyntaxHighlighter, root: *Node) !void {
        self.errors.clearRetainingCapacity();
        // Walk the AST and check for errors:
        // - Mismatched braces/brackets
        // - Unterminated strings
        // - Invalid token sequences
        // - etc.

        var line_iter = root.child;
        while (line_iter) |line| {
            try self.validateLine(line);
            line_iter = line.next;
        }
    }

    fn validateLine(self: *SyntaxHighlighter, line: *Node) !void {
        var token_iter = line.child;
        var brace_count: i32 = 0;

        while (token_iter) |token_node| {
            if (token_node.tag == .token) {
                const token = token_node.data.token;

                // Example: Check for unterminated strings
                if (token.type == .string) {
                    if (!std.mem.endsWith(u8, token.text, "\"")) {
                        self.addError(token, "Unterminated string") catch |err| {
                            Vapor.print("Error adding error: {any}\n", .{err});
                            return err;
                        };
                    }
                }

                // Track braces
                if (std.mem.eql(u8, token.text, "{")) brace_count += 1;
                if (std.mem.eql(u8, token.text, "}")) brace_count -= 1;
            }
            token_iter = token_node.next;
        }
    }

    pub fn validateJSON(self: *SyntaxHighlighter) !void {
        self.errors.clearRetainingCapacity();

        if (self.root == null) return;

        // Document-level state that persists across lines
        var brace_count: i32 = 0;
        var bracket_count: i32 = 0;
        var first_token: ?Token = null;
        var last_significant_token: ?Token = null;

        var line_iter = self.root.?.child;
        while (line_iter) |line| {
            // Per-line validation
            try self.validateLineJson(line);

            // Document-level validation (track state across lines)
            var token_iter = line.child;
            while (token_iter) |token_node| {
                if (token_node.tag == .token) {
                    const token = token_node.data.token;

                    if (token.type == .whitespace or token.type == .newline) {
                        token_iter = token_node.next;
                        continue;
                    }

                    // Track first token for root validation
                    if (first_token == null) {
                        first_token = token;
                        if (!std.mem.eql(u8, token.text, "{") and !std.mem.eql(u8, token.text, "[")) {
                            try self.addError(token, "JSON must start with '{' or '['");
                        }
                    }

                    // Count braces and brackets across entire document
                    if (std.mem.eql(u8, token.text, "{")) brace_count += 1;
                    if (std.mem.eql(u8, token.text, "}")) brace_count -= 1;
                    if (std.mem.eql(u8, token.text, "[")) bracket_count += 1;
                    if (std.mem.eql(u8, token.text, "]")) bracket_count -= 1;

                    // Trailing comma check (across line boundaries)
                    if (last_significant_token) |last| {
                        if (std.mem.eql(u8, last.text, ",")) {
                            if (std.mem.eql(u8, token.text, "}") or std.mem.eql(u8, token.text, "]")) {
                                try self.addError(last, "Trailing comma not allowed in JSON");
                            }
                        }
                    }

                    // JSON doesn't allow comments
                    if (token.type == .comment) {
                        try self.addError(token, "Comments are not allowed in JSON");
                    }

                    // JSON doesn't allow single quotes
                    if (token.type == .char) {
                        try self.addError(token, "Single quotes not allowed, use double quotes");
                    }

                    last_significant_token = token;
                }
                token_iter = token_node.next;
            }
            line_iter = line.next;
        }

        // End-of-document validation
        if (brace_count != 0 and last_significant_token != null) {
            const msg = if (brace_count > 0) "Unclosed '{'" else "Extra '}'";
            try self.addError(last_significant_token.?, msg);
        }
        if (bracket_count != 0 and last_significant_token != null) {
            const msg = if (bracket_count > 0) "Unclosed '['" else "Extra ']'";
            try self.addError(last_significant_token.?, msg);
        }
    }

    fn validateLineJson(self: *SyntaxHighlighter, line: *Node) !void {
        var token_iter = line.child;
        var last_token_type: ?TokenType = null;
        var last_token_text: ?[]const u8 = null;

        while (token_iter) |token_node| {
            if (token_node.tag == .token) {
                const token = token_node.data.token;

                if (token.type == .whitespace) {
                    token_iter = token_node.next;
                    continue;
                }

                // Check for unterminated strings
                if (token.type == .string) {
                    if (!std.mem.endsWith(u8, token.text, "\"")) {
                        try self.addError(token, "Unterminated string");
                    }
                }

                // Check for invalid identifiers (JSON only allows true/false/null)
                if (token.type == .identifier or token.type == .keyword) {
                    if (!std.mem.eql(u8, token.text, "true") and
                        !std.mem.eql(u8, token.text, "false") and
                        !std.mem.eql(u8, token.text, "null"))
                    {
                        try self.addError(token, "Invalid identifier; use true, false, or null");
                    }
                }

                // Double comma check
                if (std.mem.eql(u8, token.text, ",")) {
                    if (last_token_type == .operator and last_token_text != null) {
                        if (std.mem.eql(u8, last_token_text.?, ",")) {
                            try self.addError(token, "Unexpected comma");
                        }
                    }
                }

                // Colon must follow a string
                if (std.mem.eql(u8, token.text, ":")) {
                    if (last_token_type != .string) {
                        try self.addError(token, "':' must follow a string key");
                    }
                }

                last_token_type = token.type;
                last_token_text = token.text;
            }
            token_iter = token_node.next;
        }
    }
    fn addError(self: *SyntaxHighlighter, token: Token, message: []const u8) !void {
        try self.errors.put(token.line + token.column, .{
            .line = token.line,
            .column = token.column,
            .message = message,
        });
    }

    fn copy(highlighter: *SyntaxHighlighter) void {
        Vapor.Clipboard.copy(highlighter.local_copy_code);
        highlighter.show_cpy_btn = true;
        Vapor.registerCtxTimeout("copy_btn_editor", 500, toggleIcon, .{highlighter});
    }

    fn toggleIcon(highlighter: *SyntaxHighlighter) void {
        highlighter.show_cpy_btn = false;
        Vapor.cycle();
        // code_editor.show_cpy_btn.set(false);
    }

    pub fn renderErrors(highlighter: *SyntaxHighlighter) !void {
        var error_count: usize = 0;
        if (highlighter.errors.count() > 0) {
            List()
                .width(.percent(100))
                .height(.percent(100))
                .padding(.all(0))
                .listStyle(.none)
                .direction(.column)
                .children({
                var itr = highlighter.errors.iterator();
                while (itr.next()) |err| {
                    error_count += 1;
                    ListItem()
                        .height(.px(64))
                        .width(.percent(100))
                        .background(.hex("#CE1720"))
                        .padding(.all(4))
                        .layout(.left_center)
                        .spacing(8)
                        .children({
                        Box()
                            .padding(.all(8))
                            .layout(.center)
                            .border(.round(.black, .all(4)))
                            .background(.transparentizeHex(.black, 0.2))
                            .children({
                            Vapor.Svg(.{ .svg = svg_bug }).style(&.{
                                .size = .{ .height = .px(16), .width = .px(16) },
                                .visual = .{ .text_color = .palette(.tint), .fill = .palette(.tint) },
                            });
                        });

                        Vapor.Stack().children({
                            Text(err.value_ptr.message).style(&.{
                                .visual = .{
                                    .font_size = 15,
                                    .font_weight = 500,
                                    .text_color = .hex("#262626"),
                                    .font_style = .italic,
                                },
                            });
                            Text(Vapor.fmtln("Column {d}, Line: {d}", .{ err.value_ptr.column, err.value_ptr.line })).style(&.{
                                .visual = .{
                                    .font_size = 15,
                                    .font_weight = 500,
                                    .text_color = .hex("#262626"),
                                },
                            });
                        });
                    });
                }
            });
        }
    }

    pub fn renderAST(highlighter: *SyntaxHighlighter, node: ?*Node) !void {
        var current = node;
        while (current) |n| {
            switch (n.tag) {
                .root => {
                    Box().style(&.{
                        .position = .relative,
                        .size = .square_percent(100),
                        .direction = .column,
                        .layout = .{ .x = .start, .y = .start },
                        .visual = .{
                            // .background = .hex("#1e1e1e"),
                        },
                        .padding = .all(12),
                        // .padding = .tb(10, 10),
                    })({
                        Box().style(&.{
                            .size = .square_percent(100),
                            .scroll = .scroll_x(),
                            .direction = .column,
                            .layout = .{ .x = .start, .y = .start },
                            // .padding = .tb(10, 10),
                        })({
                            Code()({
                                try highlighter.renderAST(n.child);
                            });
                        });
                    });
                },

                .line => {
                    if (n.data.line.is_added) {
                        Box().style(&.{
                            .size = .h(.px(18)),
                            .visual = .{ .white_space = .pre, .background = .hex("#00FF00") },
                            .layout = .left_center,
                            .padding = .l(24),
                            .font_family = "DM Mono, monospace", // "IBM Plex Mono,monospace",
                        })({
                            try highlighter.renderAST(n.child);
                        });
                    } else if (n.data.line.is_removed) {
                        Box().style(&.{
                            .size = .h(.px(18)),
                            .visual = .{ .white_space = .pre, .background = .hex("#00FF00") },
                            .layout = .left_center,
                            .padding = .l(24),
                            .font_family = "DM Mono, monospace", // "IBM Plex Mono,monospace",
                        })({
                            try highlighter.renderAST(n.child);
                        });
                    } else {
                        Box().style(&.{
                            .size = .h(.px(22.5)),
                            .layout = .left_center,
                            // .padding = .l(24),
                        })({
                            Text(n.data.line.number)
                                .font(14, 600, .palette(.disabled))
                                .fontFamily("DM Mono, monospace") // "IBM Plex Mono,monospace",
                                .width(.px(24))
                                .end();
                            Box().style(&.{
                                .size = .h(.px(22.5)),
                                .visual = .{
                                    .white_space = .pre,
                                },
                                .layout = .left_center,
                                // .padding = .l(24),
                                .font_family = "DM Mono, monospace", // "IBM Plex Mono,monospace",
                            })({
                                try highlighter.renderAST(n.child);
                            });
                        });
                    }
                },
                .token => {
                    const token = n.data.token;
                    const error_annotation = highlighter.errors.get(token.line + token.column);
                    var text_decoration: ?Vapor.Types.TextDecoration = null;
                    if (error_annotation) |_| {
                        text_decoration = .{ .type = .underline, .style = .wavy, .color = .palette(.danger) };
                    }
                    switch (token.type) {
                        .keyword => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .hex("#2BACCC"),
                                },
                            });
                        },
                        .builtin => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .hex("#262626"),
                                    .text_decoration = text_decoration,
                                },
                            });
                        },
                        .operator => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_decoration = text_decoration,
                                    .text_color = .hex("#6F6F6F"),
                                },
                            });
                        },
                        .identifier => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .hex("#262626"),
                                    .text_decoration = text_decoration,
                                },
                            });
                        },
                        .component => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .palette(.code_component_color),
                                },
                            });
                        },
                        .function => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .palette(.code_function_color),
                                },
                            });
                        },
                        .comment => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .palette(.code_comment_color),
                                },
                            });
                        },
                        .string => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .palette(.code_string_color),
                                    .text_decoration = text_decoration,
                                },
                            });
                        },
                        .type_name => {
                            if (std.mem.eql(u8, token.text, "void")) {
                                Text(token.text).style(&.{
                                    .visual = .{
                                        .font_size = if (Vapor.isMobile()) 16 else 15,
                                        .font_weight = 400,
                                        .text_color = .palette(.code_type_color),
                                    },
                                });
                            } else {
                                Text(token.text).style(&.{
                                    .visual = .{
                                        .font_size = if (Vapor.isMobile()) 16 else 15,
                                        .font_weight = 400,
                                        .text_color = .hex("#262626"),
                                    },
                                });
                            }
                        },

                        else => {
                            Text(token.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = .hex("#262626"),
                                },
                            });
                        },
                    }
                    // try renderAST(n.child);
                },
                else => {},
            }

            current = n.next;
        }
    }
};

// ============================================================================
// Helper Functions
// ============================================================================

fn isKeyword(text: []const u8) bool {
    for (keywords) |kw| {
        if (std.mem.eql(u8, text, kw)) return true;
    }
    return false;
}

fn isPrimitiveType(text: []const u8) bool {
    for (primitive_types) |t| {
        if (std.mem.eql(u8, text, t)) return true;
    }
    return false;
}

fn isWhitespace(char: u8) bool {
    return char == ' ' or char == '\t' or char == '\r' or char == '\n';
}

// ============================================================================
// Debug/Testing - Print AST
// ============================================================================

pub fn printAST(node: ?*Node, writer: anytype, indent: usize) !void {
    var current = node;
    while (current) |n| {
        for (0..indent) |_| try writer.writeByte(' ');

        switch (n.tag) {
            .root => try writer.print("[Root]\n", .{}),
            .line => {
                const line_data = n.data.line;
                try writer.print("[Line {d}]\n", .{line_data.number});
            },
            .token => {
                const token = n.data.token;
                try writer.print("[{s}: \"{s}\"]\n", .{ @tagName(token.type), token.text });
            },
        }

        try printAST(n.child, writer, indent + 2);
        current = n.next;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "tokenize simple function" {
    const source =
        \\pub fn main() void {
        \\    const x = 42;
        \\}
    ;

    var tokenizer = Tokenizer.init(source);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var tokens = std.array_list.Managed(Token).init(arena.allocator());
    defer tokens.deinit();

    while (tokenizer.next()) |token| {
        try tokens.append(token);
    }

    // Verify we got tokens
    try std.testing.expect(tokens.items.len > 0);

    // First token should be 'pub' keyword
    try std.testing.expectEqualStrings("pub", tokens.items[0].text);
    try std.testing.expectEqual(TokenType.keyword, tokens.items[0].type);
}
pub inline fn Code() fn (void) void {
    const elem_decl = Vapor.ElementDecl{
        .state_type = .static,
        .elem_type = .Code,
        // .style = &.{},
    };
    _ = Vapor.LifeCycle.open(elem_decl);
    Vapor.LifeCycle.configure(elem_decl);
    return Vapor.LifeCycle.close;
}

test "parse and build AST" {
    // const source = "const x = 10;";
    const source =
        \\pub fn main() void {
        \\    const x = 42;
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var highlighter = SyntaxHighlighter.init(arena.allocator());
    try highlighter.parse(source);

    var buffer: [4096]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buffer);
    var stdout = &writer.interface;

    try printAST(highlighter.root, stdout, 0);
    stdout.flush() catch {};
}
