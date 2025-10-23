pub const packages = struct {
    pub const @"../fabric" = struct {
        pub const build_root = "/Users/vic-augustrokx-nellemann/Desktop/Zig/vaporize/../fabric";
        pub const build_zig = @import("../fabric");
        pub const deps: []const struct { []const u8, []const u8 } = &.{
        };
    };
};

pub const root_deps: []const struct { []const u8, []const u8 } = &.{
    .{ "fabric", "../fabric" },
};
