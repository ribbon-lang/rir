const std = @import("std");

const Core = @import("root.zig");

pub const Type = union(enum) {
    void: void,
    bool: void,
    u8: void, u16: void, u32: void, u64: void,
    s8: void, s16: void, s32: void, s64: void,
    f32: void, f64: void,
    pointer: Core.TypeId,
    array: Type.Array,
    product: []Core.TypeId,
    sum: Sum,
    raw_sum: []Core.TypeId,
    function: Type.Function,

    block: void,
    handler_set: void,
    evidence: Type.Function,

    const BASIC_TYPE_NAMES = [_][:0]const u8 {
        "void",
        "bool",
        "u8", "u16", "u32", "u64",
        "s8", "s16", "s32", "s64",
        "f32", "f64",
        "block",
    };

    pub const BASIC_TYPE_IDS = type_ids: {
        var type_ids = [1]Core.TypeId { undefined } ** BASIC_TYPE_NAMES.len;

        for (0..BASIC_TYPE_NAMES.len) |i| {
            type_ids[i] = @truncate(i);
        }

        break :type_ids type_ids;
    };

    pub const BASIC_TYPES = types: {
        var types = [1]Type { undefined } ** BASIC_TYPE_NAMES.len;

        for (BASIC_TYPE_NAMES, 0..) |name, i| {
            types[i] = @unionInit(Type, name, {});
        }

        break :types types;
    };

    pub const Sum = struct {
        discriminator: Core.TypeId,
        types: []Core.TypeId,

        pub fn clone(self: *const Type.Sum, allocator: std.mem.Allocator) !Type.Sum {
            return Type.Sum {
                .discriminator = self.discriminator,
                .types = try allocator.dupe(Core.TypeId, self.types),
            };
        }

        pub fn deinit(self: Type.Sum, allocator: std.mem.Allocator) void {
            allocator.free(self.types);
        }
    };

    pub const Array = struct {
        length: usize,
        element: Core.TypeId,
    };

    pub const Function = struct {
        return_type: Core.TypeId,
        termination_type: Core.TypeId,
        effects: []Core.EvidenceId,
        parameter_types: []Core.TypeId,

        pub fn deinit(self: Type.Function, allocator: std.mem.Allocator) void {
            allocator.free(self.effects);
            allocator.free(self.parameter_types);
        }

        pub fn clone(self: *const Type.Function, allocator: std.mem.Allocator) !Type.Function {
            const effects = try allocator.dupe(Core.EvidenceId, self.effects);
            errdefer allocator.free(effects);

            const parameter_types = try allocator.dupe(Core.TypeId, self.parameter_types);
            errdefer allocator.free(parameter_types);

            return Type.Function {
                .return_type = self.return_type,
                .termination_type = self.termination_type,
                .effects = effects,
                .parameter_types = parameter_types,
            };
        }
    };

    pub fn clone(self: *const Type, allocator: std.mem.Allocator) !Type {
        switch (self.*) {
            .product => |field_types| return Type { .product = try allocator.dupe(Core.TypeId, field_types) },
            .sum => |sum| return Type { .sum = try sum.clone(allocator) },
            .raw_sum => |field_types| return Type { .raw_sum = try allocator.dupe(Core.TypeId, field_types) },
            .function => |fun| return Type { .function = try fun.clone(allocator) },
            .evidence => |fun| return Type { .evidence = try fun.clone(allocator) },
            inline else => return self.*,
        }
    }

    pub fn deinit(self: Type, allocator: std.mem.Allocator) void {
        switch (self) {
            .product => |field_types| allocator.free(field_types),
            .sum => |sum| sum.deinit(allocator),
            .raw_sum => |field_types| allocator.free(field_types),
            .function => |fun| fun.deinit(allocator),
            .evidence => |fun| fun.deinit(allocator),
            inline else => {},
        }
    }

    pub fn asFunction(self: Type) !Type.Function {
        switch (self) {
            .function => |fun| return fun,
            .evidence => |fun| return fun,
            inline else => return error.InvalidType,
        }
    }
};
