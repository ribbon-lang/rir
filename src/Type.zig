const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;

const IR = @import("root.zig");


pub const Type = union(enum) {
    void: void,
    bool: void,
    u8: void, u16: void, u32: void, u64: void,
    s8: void, s16: void, s32: void, s64: void,
    f32: void, f64: void,
    pointer: IR.TypeId,
    array: Type.Array,
    product: []IR.TypeId,
    sum: Sum,
    raw_sum: []IR.TypeId,
    function: Type.Function,

    block: void,
    handler_set: void,
    evidence: Type.Function,

    pub const Map = struct {
        allocator: std.mem.Allocator,
        inner: std.ArrayHashMapUnmanaged(IR.Type, void, MiscUtils.SimpleHashContext, true) = .{},

        /// Allocator provided should be an arena or a similar allocator,
        /// that does not care about freeing individual allocations
        pub fn init(allocator: std.mem.Allocator) !Map {
            return Map { .allocator = allocator };
        }

        /// Calls `IR.Type.clone` on the input, if the type is not found in the map
        pub fn typeId(self: *Map, ty: IR.Type) !IR.TypeId {
            if (self.inner.getIndex(ty)) |index| {
                return @truncate(index);
            }

            const index = self.inner.count();

            if (index >= IR.MAX_TYPES) {
                return error.TooManyTypes;
            }

            try self.inner.put(self.allocator, try ty.clone(self.allocator), {});

            return @truncate(index);
        }

        /// Does not call `IR.Type.clone` on the input
        pub fn typeIdPreallocated(self: *Map, ty: IR.Type) !IR.TypeId {
            if (self.inner.getIndex(ty)) |index| {
                return @truncate(index);
            }

            const index = self.inner.count();

            if (index >= IR.MAX_TYPES) {
                return error.TooManyTypes;
            }

            try self.inner.put(self.allocator, ty, {});

            return @truncate(index);
        }

        pub fn typeFromNative(self: *const Map, comptime T: type) !IR.Type {
            switch (T) {
                void => return .void,
                bool => return .bool,
                u8 => return .u8, u16 => return .u16, u32 => return .u32, u64 => return .u64,
                i8 => return .s8, i16 => return .s16, i32 => return .s32, i64 => return .s64,
                f32 => return .f32, f64 => return .f64,

                else => switch (@typeInfo(T)) {
                    .pointer => |info| return IR.Type { .pointer = try self.typeIdFromNative(info.child) },
                    .array => |info| return IR.Type { .array = .{
                        .length = info.len,
                        .element = try self.typeIdFromNative(info.child),
                    } },
                    .@"struct" => |info| {
                        var field_types = self.allocator.alloc(IR.TypeId, info.fields.len);
                        errdefer self.allocator.free(field_types);

                        for (info.fields, 0..) |field, i| {
                            field_types[i] = try self.typeIdFromNative(field.type);
                        }

                        return IR.Type { .product = field_types };
                    },
                    .@"enum" => |info| return self.typeFromNative(info.tag_type),
                    .@"union" => |info| {
                        var field_types = self.allocator.alloc(IR.TypeId, info.fields.len);
                        errdefer self.allocator.free(field_types);

                        for (info.fields, 0..) |field, i| {
                            field_types[i] = try self.typeIdFromNative(field.type);
                        }

                        if (info.tag_type) |TT| {
                            return IR.Type { .sum = .{
                                .discriminator = try self.typeIdFromNative(TT),
                                .types = field_types,
                            } };
                        } else {
                            return IR.Type { .raw_sum = field_types };
                        }
                    },
                    .@"fn" => |info| {
                        const return_type = try self.typeIdFromNative(info.return_type.?);
                        const termination_type = try self.typeIdFromNative(void);

                        const effects = self.allocator.alloc(IR.EvidenceId, 0);
                        errdefer self.allocator.free(effects);

                        var parameter_types = self.allocator.alloc(IR.TypeId, info.param_info.len);
                        errdefer self.allocator.free(parameter_types);

                        for (info.param_info, 0..) |param, i| {
                            parameter_types[i] = try self.typeIdFromNative(param.type);
                        }

                        return IR.Type { .function = .{
                            .return_type = return_type,
                            .termination_type = termination_type,
                            .effects = effects,
                            .parameter_types = parameter_types,
                        } };
                    },

                    else => @compileError("cannot convert type `" ++ @typeName(T) ++ "` to IR.Type"),
                }
            }
        }

        pub fn typeIdFromNative(self: *Map, comptime T: type) !IR.TypeId {
            const ty = try self.typeFromNative(T);
            errdefer IR.Type.deinit(ty, self.allocator);

            return self.typeIdPreallocated(ty);
        }

        pub fn getType(self: *Map, id: IR.TypeId) !IR.Type {
            if (id >= self.inner.count()) {
                return error.InvalidType;
            }

            return self.inner.keys()[id];
        }
    };

    const BASIC_TYPE_NAMES = [_][:0]const u8 {
        "void",
        "bool",
        "u8", "u16", "u32", "u64",
        "s8", "s16", "s32", "s64",
        "f32", "f64",
        "block",
    };

    pub const BASIC_TYPE_IDS = type_ids: {
        var type_ids = [1]IR.TypeId { undefined } ** BASIC_TYPE_NAMES.len;

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
        discriminator: IR.TypeId,
        types: []IR.TypeId,

        pub fn clone(self: *const Type.Sum, allocator: std.mem.Allocator) !Type.Sum {
            return Type.Sum {
                .discriminator = self.discriminator,
                .types = try allocator.dupe(IR.TypeId, self.types),
            };
        }

        pub fn deinit(self: Type.Sum, allocator: std.mem.Allocator) void {
            allocator.free(self.types);
        }
    };

    pub const Array = struct {
        length: usize,
        element: IR.TypeId,
    };

    pub const Function = struct {
        return_type: IR.TypeId,
        termination_type: IR.TypeId,
        effects: []IR.EvidenceId,
        parameter_types: []IR.TypeId,

        pub fn deinit(self: Type.Function, allocator: std.mem.Allocator) void {
            allocator.free(self.effects);
            allocator.free(self.parameter_types);
        }

        pub fn clone(self: *const Type.Function, allocator: std.mem.Allocator) !Type.Function {
            const effects = try allocator.dupe(IR.EvidenceId, self.effects);
            errdefer allocator.free(effects);

            const parameter_types = try allocator.dupe(IR.TypeId, self.parameter_types);
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
            .product => |field_types| return Type { .product = try allocator.dupe(IR.TypeId, field_types) },
            .sum => |sum| return Type { .sum = try sum.clone(allocator) },
            .raw_sum => |field_types| return Type { .raw_sum = try allocator.dupe(IR.TypeId, field_types) },
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
