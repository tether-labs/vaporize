const std = @import("std");
const Vapor = @import("vapor");
const Static = Vapor.Static;
const CtxButton = Static.CtxButton;
const Box = Static.Box;
const Graphic = Static.Graphic;
const Icon = Vapor.Icon;
const Text = Static.Text;
const Color = Vapor.Types.Color;
// const code = @import("code/FormCode.zig").code;

const Signal = Vapor.Signal;

const NewLine = struct {
    processed_text: []TextDetails = undefined,
    is_removed: bool = false,
    is_added: bool = false,
};

const TextDetails = struct {
    color: Color = .palette(.code_text_color),
    text: []const u8 = "",
};

var text_color: [4]f32 = undefined;

const CodeEditor = @This();
allocator: *std.mem.Allocator = undefined,
processed_lines: std.array_list.Managed(NewLine),
// show_cpy_btn: Signal(bool) = undefined,
show_cpy_btn: bool = false,
local_copy_code: []const u8 = undefined,

fn toggleIcon(code_editor: *CodeEditor) void {
    code_editor.show_cpy_btn = false;
    Vapor.cycle();
    // code_editor.show_cpy_btn.set(false);
}

fn copy(code_editor: *CodeEditor) void {
    Vapor.Clipboard.copy(code_editor.local_copy_code);
    code_editor.show_cpy_btn = true;
    Vapor.cycle();
    Vapor.registerCtxTimeout("copy_btn_editor", 500, toggleIcon, .{code_editor});
}

pub fn initWrapper(ptr: *anyopaque, allocator: *std.mem.Allocator, code: []const u8) void {
    const self: *CodeEditor = @ptrCast(@alignCast(ptr));
    self.init(allocator, code);
}

pub fn init(target: *CodeEditor, allocator: *std.mem.Allocator, code: []const u8) void {
    target.* = CodeEditor{
        .allocator = allocator,
        .processed_lines = std.array_list.Managed(NewLine).init(allocator.*),
    };
    // target.show_cpy_btn.init(false);
    target.local_copy_code = code;
    target.tokenize(code) catch |err| {
        Vapor.println("Tokenize error {any}\n", .{err});
        return;
    };
}

pub fn reinit(code_editor: *CodeEditor, code: []const u8) !void {
    code_editor.local_copy_code = code;
    code_editor.processed_lines = std.array_list.Managed(NewLine).init(code_editor.allocator.*);
    try code_editor.tokenize(code);
}

pub fn deinit(code_editor: *CodeEditor) void {
    for (code_editor.processed_lines.items) |line| {
        for (line.processed_text) |details| {
            if (!std.mem.eql(u8, details.text, "\n")) {
                code_editor.allocator.free(details.text);
            }
        }
        code_editor.allocator.free(line.processed_text);
        // line.processed_text.deinit();
    }
    code_editor.processed_lines.deinit();
    // code_editor.show_cpy_btn.deinit();
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
pub fn render(code_editor: *CodeEditor, _: f32) void {
    Box().style(&.{
        .size = .square_percent(100),
        .scroll = .scroll_y(),
        .show_scrollbar = false,
        .direction = .column,
        .layout = .{ .x = .start, .y = .start },
        .visual = .{
            .background = .palette(.code_background),
            .border = .simple(.palette(.disabled)),
            // .border_radius = .all(8),
        },
        .padding = .tb(10, 10),
    })({
        Box().style(&.{
            .layout = .x_between_center,
            .size = .w(.percent(100)),
            .padding = .tblr(0, 8, 12, 12),
            .visual = .{
                .border = .bottom(.palette(.disabled)),
            },
        })({
            Box().style(&.{
                .size = .h(.percent(100)),
                .child_gap = 8,
            })({
                // Box().style(&.{
                //     .size = .square_px(14),
                //     .visual = .{ .border_radius = .all(0), .background = .hex("#FF0000") },
                // })({});
                // Box().style(&.{
                //     .size = .square_px(14),
                //     .visual = .{ .border_radius = .all(0), .background = .hex("#FFFF00") },
                // })({});
                // Box().style(&.{
                //     .size = .square_px(14),
                //     .visual = .{ .border_radius = .all(0), .background = .hex("#09FF00") },
                // })({});
                Text("Example").font(14, 600, .palette(.text_color)).end();
            });
            Box().style(&.{
                .size = .w(.percent(10)),
                .layout = .x_even_center,
            })({
                CtxButton(copy, .{code_editor})
                    // .tooltip(&.{
                    //     .text = "Copy",
                    //     .position = .bottom,
                    //     .layout = .center,
                    //     .color = .palette(.text_color),
                    //     .delay = 100,
                    // })
                    .style(&.{
                    .position = .relative,
                    .size = .square_px(22),
                    .transition = .{ .duration = 100 },
                    .visual = .{
                        .border_radius = .all(4),
                        .background = .transparent,
                        .text_color = .palette(.disabled),
                        .cursor = .pointer,
                    },
                    .interactive = .{
                        .hover = .{ .text_color = .palette(.text_color) },
                    },
                    .layout = .center,
                })({
                    if (code_editor.show_cpy_btn) {
                        Icon(.check).style(&.{
                            .visual = .{ .font_size = 16 },
                        });
                    } else {
                        Icon(.clipboard).style(&.{
                            .visual = .{ .font_size = 16 },
                        });
                    }
                });
                // Icon("bi bi-code").style(&.{
                //     .visual = .{
                //         .font_size = 16,
                //         .text_color = .palette(.disabled),
                //     },
                // });
                Graphic(.{ .src = "/src/assets/zig_simple.svg" }).style(&.{
                    .size = .{ .height = .px(16), .width = .px(16) },
                    .visual = .{ .text_color = .palette(.tint), .fill = .palette(.tint) },
                });
            });
        });
        Box().style(&.{
            .size = .square_percent(100),
            .scroll = .scroll_x(),
            .direction = .column,
            .layout = .{ .x = .start, .y = .start },
            .padding = .tb(10, 10),
        })({
            Code()({
                for (code_editor.processed_lines.items) |line| {
                    const color = if (line.is_removed) blk: {
                        break :blk Vapor.Types.Background.transparentizeHex(.hex("#FF0000"), 0.1);
                    } else if (line.is_added) blk: {
                        break :blk Vapor.Types.Background.transparentizeHex(.hex("#00FF00"), 0.1);
                    } else blk: {
                        break :blk Vapor.Types.Background.transparent;
                    };
                    Box().style(&.{
                        .size = .h(.px(18)),
                        .white_space = .pre,
                        .layout = .left_center,
                        .padding = .l(24),
                        .visual = .{ .background = color },
                        .font_family = "DM Mono, monospace", // "IBM Plex Mono,monospace",
                        // .font_family = "IBM Plex Mono,monospace",
                    })({
                        for (line.processed_text) |word| {
                            Text(word.text).style(&.{
                                .visual = .{
                                    .font_size = if (Vapor.isMobile()) 16 else 15,
                                    .font_weight = 400,
                                    .text_color = word.color,
                                },
                            });
                        }
                    });
                }
            });
        });
    });
}

const Declarations = enum {
    @"const",
    @"var",
    @"defer",
    @"while",
    @"fn",
    @"switch",
    @"return",
    @"break",
    @"struct",
    @"enum",
    @"union",
    @"export",
    @"extern",
    @"try",
    @"if",
    @"else",
    @"pub",
    @"for",
};

pub fn includes(haystack: []const u8, needle: []const u8) bool {
    // std.mem.indexOf returns the index of the first occurrence of `needle`,
    // or `null` if it's not found. We just need to check if the result is not null.
    return std.mem.indexOf(u8, haystack, needle) != null;
}

pub fn containsAlphabetic(input: []const u8) bool {
    for (input) |char| {
        if (std.ascii.isAlphabetic(char)) {
            return true;
        }
    }
    return false;
}

fn appendPeriod(processed_text: *std.array_list.Managed(TextDetails)) !void {
    try processed_text.append(.{
        .text = ".",
    });
}

fn parseSubText(allocator: *std.mem.Allocator, processed_text: *std.array_list.Managed(TextDetails), sub_text: []const u8) !void {
    var period_itr = std.mem.tokenizeScalar(u8, sub_text, '.');
    const count = std.mem.count(u8, sub_text, ".");
    var period_counter: usize = 0;
    const count_bracket_open = std.mem.count(u8, sub_text, "(");
    const count_bracket_closed = std.mem.count(u8, sub_text, ")");
    const is_string = std.mem.count(u8, sub_text, "\"");
    // Here we chekc if the line has an open bracket and a period
    // Signal(bool).init(false, . = 1 ( = 2
    // Sl-1: Signal(bool)
    // Sl-2: init(false,
    if (count > 0 and count_bracket_open > 0) {
        while (period_itr.next()) |sub_slice| {
            if (period_counter > 0 and count > 1) {
                appendPeriod(processed_text) catch return;
            } else if (count == 1 and period_itr.peek() == null) {
                appendPeriod(processed_text) catch return;
            } else if (sub_text[0] == '.') {
                appendPeriod(processed_text) catch return;
            }
            var text_deets = TextDetails{};
            if (sub_slice[0] == '@') {
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_text});
                try processed_text.append(text_deets);
                return;
            }
            // text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
            // try processed_text.append(text_deets);
            const includes_bracket = std.mem.indexOf(u8, sub_slice, "(");
            const includes_close_bracket = std.mem.indexOf(u8, sub_slice, ")");
            // Here we check if the subslice contains the open bracket and its not the first one
            if (includes_bracket != null and includes_bracket != 0 and includes_close_bracket != null and count_bracket_open == count_bracket_closed and containsAlphabetic(sub_slice)) {
                var split = std.mem.splitScalar(u8, sub_slice, '(');
                text_deets.color = .palette(.code_function_color);
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{split.next().?});
                var text_deets_second = TextDetails{};
                const result_sec = try std.fmt.allocPrint(allocator.*, "({s}", .{split.next().?});
                text_deets_second.text = result_sec;
                try processed_text.append(text_deets);
                try processed_text.append(text_deets_second);
            } else if (includes_bracket != null and includes_bracket != 0 and containsAlphabetic(sub_slice)) {
                var split = std.mem.splitScalar(u8, sub_slice, '(');
                text_deets.color = .palette(.code_function_color);
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{split.next().?});
                try processed_text.append(text_deets);
                while (split.next()) |sub_sub_slice| {
                    var text_deets_second = TextDetails{};
                    text_deets_second.text = try std.fmt.allocPrint(allocator.*, "({s}", .{sub_sub_slice});
                    try processed_text.append(text_deets_second);
                }
                // const result_sec = try std.fmt.allocPrint(allocator.*, "({s}", .{split.next().?});
                // text_deets_second.text = result_sec;
            } else if (includes_bracket != null and includes_bracket != 0) {
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
                try processed_text.append(text_deets);
            } else if (includes(sub_slice, "Box")) {
                // text_deets.color = Vapor.hexToRgba("#E5FF54");
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
                try processed_text.append(text_deets);
            } else if (includes(sub_slice, "Text")) {
                // text_deets.color = Vapor.hexToRgba("#E5FF54");
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
                try processed_text.append(text_deets);
            } else {
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
                try processed_text.append(text_deets);
            }

            // else if (period_itr.peek() == null and count > period_counter) {
            //     // Here we check if the counter inlcudes more periods if there is only one
            //     // we append the subsilce with a period
            //     text_deets.text = try std.fmt.allocPrint(allocator.*, ".{s}", .{sub_slice});
            //     try processed_text.append(text_deets);
            //     period_counter += 1;
            // } else if (count > period_counter) {
            //     text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}.", .{sub_slice});
            //     try processed_text.append(text_deets);
            //     period_counter += 1;
            // } else {
            //     text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
            //     try processed_text.append(text_deets);
            // }
            period_counter += 1;
        }
    } else if (count > 0 and is_string == 0 and count_bracket_open == 0 and count_bracket_closed == 0) {
        while (period_itr.next()) |sub_slice| {
            var text_deets = TextDetails{};
            if (period_itr.peek() == null) {
                if (period_itr.peek() == null and period_counter < count) {
                    text_deets.text = try std.fmt.allocPrint(allocator.*, ".{s}", .{sub_slice});
                    period_counter += 1;
                } else {
                    // text_deets.color = .{ 184, 187, 221, 255 };
                    text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_slice});
                }
            } else {
                period_counter += 1;
                text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}.", .{sub_slice});
            }
            try processed_text.append(text_deets);
        }
    } else {
        var text_deets = TextDetails{};
        if (period_itr.peek() == null and period_counter < count) {
            text_deets.text = try std.fmt.allocPrint(allocator.*, ".{s}", .{sub_text});
            period_counter += 1;
        } else {
            text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{sub_text});
        }
        try processed_text.append(text_deets);
    }
}

// Each ident is a block element itself
// this we we can use the column direction to organize everything
// every ident results in the text line having increased padding
pub fn tokenize(code_editor: *CodeEditor, text: []const u8) !void {
    const allocator: *std.mem.Allocator = code_editor.allocator;
    var depth: usize = 0;
    var first_word: bool = false;
    var is_comment: bool = false;
    // Iterate throught the lines
    var line_itr = std.mem.splitSequence(u8, text, "\n");
    // Then we iterate through each word of the line
    outer: while (line_itr.next()) |line| {
        first_word = true;
        // var word_count: usize = 0;
        var processed_texts: std.array_list.Managed(TextDetails) = std.array_list.Managed(TextDetails).init(allocator.*);
        var word_itr = std.mem.tokenizeScalar(u8, line, ' ');
        const brace_open_count: usize = @intCast(std.mem.count(u8, line, "{"));
        const brace_close_count: usize = @intCast(std.mem.count(u8, line, "}"));
        //
        if (line.len == 0) {
            var text_deets = TextDetails{};
            text_deets.text = "\n";
            try processed_texts.append(text_deets);
            const new_line = NewLine{
                .processed_text = try processed_texts.toOwnedSlice(),
            };
            try code_editor.processed_lines.append(new_line);
            continue :outer;
        }

        while (word_itr.next()) |word| {
            if (std.mem.eql(u8, word, "//") or std.mem.eql(u8, word, "///")) {
                is_comment = true;
            }
            var text_deets = TextDetails{};

            var buf = std.array_list.Managed(u8).init(allocator.*);
            if (first_word and word_itr.peek() == null and depth > 0 and !containsAlphabetic(word)) {
                depth -= 1;
                try buf.appendNTimes(' ', @intCast(depth * 4));
                depth += 1;
            } else if (first_word and depth > 0) {
                try buf.appendNTimes(' ', @intCast(depth * 4));
            }

            first_word = false;
            const padding = try buf.toOwnedSlice();
            const result = try std.fmt.allocPrint(allocator.*, "{s}{s} ", .{ padding, word });
            allocator.free(padding);
            const is_decl = std.meta.stringToEnum(Declarations, word);
            if (!is_comment) {
                if (is_decl) |decl| {
                    switch (decl) {
                        Declarations.@"const",
                        Declarations.@"var",
                        Declarations.@"defer",
                        Declarations.@"while",
                        Declarations.@"fn",
                        Declarations.@"switch",
                        Declarations.@"try",
                        Declarations.@"if",
                        Declarations.@"else",
                        Declarations.@"pub",
                        Declarations.@"return",
                        Declarations.@"break",
                        Declarations.@"struct",
                        Declarations.@"enum",
                        Declarations.@"union",
                        Declarations.@"export",
                        Declarations.@"extern",
                        Declarations.@"for",
                        => {
                            text_deets.color = .palette(.code_tint_color);
                        },
                    }
                    text_deets.text = result;
                    try processed_texts.append(text_deets);
                } else if (includes(result, ".")) {
                    // Here we parse subtext
                    try parseSubText(allocator, &processed_texts, result);
                    allocator.free(result);
                } else if (includes(result, "(") and containsAlphabetic(result)) {
                    // Vapor.println("Found bracket ( {s}", .{result});
                    var split = std.mem.splitScalar(u8, result, '(');
                    text_deets.color = .palette(.code_keyword_color);
                    text_deets.text = try std.fmt.allocPrint(allocator.*, "{s}", .{split.next().?});
                    try processed_texts.append(text_deets);
                    var text_deets_second = TextDetails{};
                    const result_sec = try std.fmt.allocPrint(allocator.*, "({s}", .{split.next().?});
                    text_deets_second.text = result_sec;
                    try processed_texts.append(text_deets_second);
                } else {
                    text_deets.text = result;
                    try processed_texts.append(text_deets);
                }
            } else {
                text_deets.text = result;
                // text_deets.color = Vapor.hexToRgba("#88859D");
                try processed_texts.append(text_deets);
            }
        }
        const new_line = NewLine{
            .processed_text = try processed_texts.toOwnedSlice(),
            .is_removed = if (includes(line, "--")) true else false,
            .is_added = if (includes(line, "++")) true else false,
        };
        try code_editor.processed_lines.append(new_line);

        is_comment = false;
        depth += brace_open_count;
        depth -= brace_close_count;
        // depth += @intCast(std.mem.count(u8, line, "{"));
        // depth -= @intCast(std.mem.count(u8, line, "{}"));
    }
}
