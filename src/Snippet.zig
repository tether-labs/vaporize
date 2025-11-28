const Vapor = @import("vapor");
const std = @import("std");
const Static = Vapor.Static;
const Box = Static.Box;
const Text = Static.Text;
const CtxButton = Static.CtxButton;
const Icon = Static.Icon;
const Color = Vapor.Types.Color;

const Snippet = @This();
copied: bool = false,
copied_text: []const u8 = "",

pub fn init(snippet: *Snippet) void {
    snippet.* = .{};
}

fn copy(snippet: *Snippet, text: []const u8) void {
    Vapor.Clipboard.copy(text);
    snippet.copied = true;
    snippet.copied_text = text;
    Vapor.cycle();
    Vapor.registerCtxTimeout("copy_snippet", 500, toggleIcon, .{snippet});
}

fn toggleIcon(snippet: *Snippet) void {
    snippet.copied = false;
    snippet.copied_text = "";
    Vapor.cycle();
}

pub fn code(snippet: *Snippet, text: []const u8) void {
    CtxButton(copy, .{ snippet, text })
        .style(&.{
        .visual = .{
            .border = .simple(.palette(.border_color_light)),
            .text_color = .palette(.text_color),
            .cursor = .pointer,
            .background = .palette(.background),
            .shadow = .card(.transparentizeHex(.palette(.tint), 0.2)),
        },
        .padding = .all(8),
        .size = .square_percent(100),
        .child_gap = 16,
        .layout = .flex,
        .interactive = .{
            .hover = .{ .text_color = .palette(.tint), .border = .{ .color = .palette(.tint) } },
        },
        .position = .relative,
    })({
        Text(text)
            .font(16, null, null)
            .ellipsis(.dot)
            .fontFamily("Azeret Mono, monospace")
            .layout(.center)
            .size(.w(.grow))
            .end();
        Box().style(&.{
            // .position = .{ .type = .absolute, .right = .px(8), .top = .px(8) },
            .size = .square_px(22),
            .transition = .{ .duration = 100 },
            .visual = .{
                .border_radius = .all(4),
                .background = .transparent,
                .cursor = .pointer,
            },
            .layout = .center,
        })({
            if (snippet.copied and std.mem.eql(u8, text, snippet.copied_text)) {
                Icon(.check).style(&.{
                    .visual = .{ .font_size = 16 },
                });
            } else {
                Icon(.clipboard).style(&.{
                    .visual = .{ .font_size = 16 }, // we need to fix this to make sure it does not repalce the class of the text below
                    // setting it to 16 results in the Text below being overwritten
                });
            }
        });
    });
}
