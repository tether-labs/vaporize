const std = @import("std");
const Vapor = @import("vapor");
const Self = @import("Vaporize.zig").Self;
const Node = @import("Vaporize.zig").Node;
const getUnderlyingType = @import("Vaporize.zig").getUnderlyingType;
const getUnderlyingValue = @import("Vaporize.zig").getUnderlyingValue;
const FormComponent = @import("vapor").Form;
const SubmitButton = @import("vapor").SubmitButton;
const Text = @import("vapor").Text;
const Box = @import("vapor").Box;
const Style = @import("vapor").Style;

pub var current_parent: ?*Node = null;
pub fn Form(self: *Self, comptime T: type) type {
    return struct {
        const Vaporize = @This();

        const InnerForm = struct {
            form_data: T,
            validation_error: ?*ValidationResult = null,
        };

        self: *Self,
        inner_form: *InnerForm,

        pub fn compile(vaporize: *Vaporize) !void {
            const allocator = Vapor.arena(.persist);
            const inner_self = allocator.create(Self) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
                unreachable;
            };
            inner_self.* = try Self.init(allocator, self.style_config);
            const inner_form = try allocator.create(InnerForm);
            const validation_error = allocator.create(ValidationResult) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
                unreachable;
            };
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

            const root = try vaporize.self.createNode(.Form);
            current_parent = null;
            vaporize.self.root = root;
            field_index = 0;
            vaporize.self.generateForm(vaporize.self.root, "root", T, {}, T, vaporize.inner_form) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
            };
        }
        pub fn render(vaporize: *const Vaporize) void {
            vaporize.self.traverseForm(vaporize.self.root, vaporize.inner_form) catch |err| {
                Vapor.printErr("Error: {any}\n", .{err});
            };
        }
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
                        .telephone => validatePhone(str_value),
                        // .alphanumeric => validateAlphanumeric(str_value),
                        else => true,
                    };

                    if (!type_valid) {
                        const msg = switch (field_type) {
                            .email => "Must be a valid email address",
                            .password => "Password must be at least 8 characters with uppercase, lowercase, and numbers",
                            // .url => "Must be a valid URL",
                            .telephone => "Must be a valid phone number",
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

var field_index: usize = 0;
pub fn generateForm(self: *Self, parent: *Node, field_name: []const u8, comptime VT: type, value: anytype, comptime form_type: type, form_value: anytype) !void {
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

pub inline fn traverseForm(self: *Self, node: ?*Node, value: anytype) !void {
    var current = node;
    while (current) |n| {
        switch (n.tag) {
            .Form => {
                FormComponent(validateForm, value).style(&.{
                    .size = .{ .width = .percent(100), .height = .percent(100) },
                    .padding = .tb(8, 8),
                    .layout = .center,
                    .direction = .column,
                    .child_gap = 16,
                })({
                    try self.traverse(n.child, .{}, value);

                    const button_style: Vapor.Types.Style = self.style_config.button_style;
                    SubmitButton().style(&button_style)({
                        Text("Submit").end();
                    });
                });
            },
            else => {},
        }
        current = n.next;
    }
}

pub fn traverseFormValue(self: *Self, node: ?*Node, style: Style, value: anytype) !void {
    var current = node;
    while (current) |n| {
        switch (n.tag) {
            .Root => {
                try self.traverse(n.child, style, value);
            },
            else => {},
        }
        current = n.next;
    }
}
