//! By convention, root.zig is the root source file when making a library.
pub const Parser = @import("Vaporize.zig").Parser;
pub const Node = @import("Vaporize.zig").Node;
pub const traverse = @import("Vaporize.zig").traverse;
