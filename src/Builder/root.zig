const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;

const Core = @import("Core");


pub const BlockBuilder = @import("BlockBuilder.zig");
pub const FunctionBuilder = @import("FunctionBuilder.zig");
pub const HandlerSetBuilder = @import("HandlerSetBuilder.zig");


const Builder = @This();


allocator: std.mem.Allocator,
type_map: std.ArrayHashMapUnmanaged(Core.Type, void, MiscUtils.SimpleHashContext, true) = .{},
global_list: std.ArrayListUnmanaged(Core.Global) = .{},
function_list: std.ArrayListUnmanaged(*FunctionBuilder) = .{},
foreign_function_list: std.ArrayListUnmanaged(Core.ForeignFunction) = .{},
evidence_list: std.ArrayListUnmanaged(Core.TypeId) = .{},

/// Allocator provided should be an arena or a similar allocator,
/// that does not care about freeing individual allocations
pub fn init(allocator: std.mem.Allocator) Builder {
    return Builder {
        .allocator = allocator,
    };
}

/// Calls `Core.Type.clone` on the input, if the type is not found in the map
pub fn typeId(self: *Builder, ty: Core.Type) !Core.TypeId {
    if (self.type_map.getIndex(ty)) |index| {
        return @truncate(index);
    }

    const index = self.type_map.count();

    if (index >= Core.MAX_TYPES) {
        return error.TooManyTypes;
    }

    try self.type_map.put(self.allocator, try ty.clone(self.allocator), {});

    return @truncate(index);
}

/// Does not call `Core.Type.clone` on the input
pub fn typeIdPreallocated(self: *Builder, ty: Core.Type) !Core.TypeId {
    if (self.type_map.getIndex(ty)) |index| {
        return @truncate(index);
    }

    const index = self.type_map.count();

    if (index >= Core.MAX_TYPES) {
        return error.TooManyTypes;
    }

    try self.type_map.put(self.allocator, ty, {});

    return @truncate(index);
}

pub fn typeFromNative(self: *const Builder, comptime T: type) !Core.Type {
    switch (T) {
        void => return .void,
        bool => return .bool,
        u8 => return .u8, u16 => return .u16, u32 => return .u32, u64 => return .u64,
        i8 => return .s8, i16 => return .s16, i32 => return .s32, i64 => return .s64,
        f32 => return .f32, f64 => return .f64,

        else => switch (@typeInfo(T)) {
            .pointer => |info| return Core.Type { .pointer = try self.typeIdFromNative(info.child) },
            .array => |info| return Core.Type { .array = .{
                .length = info.len,
                .element = try self.typeIdFromNative(info.child),
            } },
            .@"struct" => |info| {
                var field_types = self.allocator.alloc(Core.TypeId, info.fields.len);
                errdefer self.allocator.free(field_types);

                for (info.fields, 0..) |field, i| {
                    field_types[i] = try self.typeIdFromNative(field.type);
                }

                return Core.Type { .product = field_types };
            },
            .@"enum" => |info| return self.typeFromNative(info.tag_type),
            .@"union" => |info| {
                var field_types = self.allocator.alloc(Core.TypeId, info.fields.len);
                errdefer self.allocator.free(field_types);

                for (info.fields, 0..) |field, i| {
                    field_types[i] = try self.typeIdFromNative(field.type);
                }

                if (info.tag_type) |TT| {
                    return Core.Type { .sum = .{
                        .discriminator = try self.typeIdFromNative(TT),
                        .types = field_types,
                    } };
                } else {
                    return Core.Type { .raw_sum = field_types };
                }
            },
            .@"fn" => |info| {
                const return_type = try self.typeIdFromNative(info.return_type.?);
                const termination_type = try self.typeIdFromNative(void);

                const effects = self.allocator.alloc(Core.EvidenceId, 0);
                errdefer self.allocator.free(effects);

                var parameter_types = self.allocator.alloc(Core.TypeId, info.param_info.len);
                errdefer self.allocator.free(parameter_types);

                for (info.param_info, 0..) |param, i| {
                    parameter_types[i] = try self.typeIdFromNative(param.type);
                }

                return Core.Type { .function = .{
                    .return_type = return_type,
                    .termination_type = termination_type,
                    .effects = effects,
                    .parameter_types = parameter_types,
                } };
            },

            else => @compileError("cannot convert type `" ++ @typeName(T) ++ "` to Core.Type"),
        }
    }
}

pub fn typeIdFromNative(self: *Builder, comptime T: type) !Core.TypeId {
    const ty = try self.typeFromNative(T);
    errdefer Core.Type.deinit(ty, self.allocator);

    return self.typeIdPreallocated(ty);
}

pub fn getType(self: *Builder, id: Core.TypeId) !Core.Type {
    if (id >= self.type_map.count()) {
        return error.InvalidType;
    }

    return self.type_map.keys()[id];
}

/// Calls `allocator.dupe` on the input bytes
pub fn globalFromBytes(self: *Builder, tyId: Core.TypeId, bytes: []const u8) !Core.GlobalId {
    const dupeBytes = try self.allocator.dupe(u8, bytes);
    errdefer self.allocator.free(dupeBytes);

    return self.globalFromBytesPreallocated(tyId, dupeBytes);
}

/// Does not call `allocator.dupe` on the input bytes
pub fn globalFromBytesPreallocated(self: *Builder, tyId: Core.TypeId, bytes: []u8) !Core.GlobalId {
    const index = self.global_list.items.len;

    if (index >= Core.MAX_GLOBALS) {
        return error.TooManyGlobals;
    }

    const global = Core.Global {
        .id = @truncate(index),
        .type = tyId,
        .value = bytes,
    };

    try self.global_list.append(self.allocator, global);

    return global.id;
}

pub fn globalFromNative(self: *Builder, value: anytype) !Core.GlobalId {
    const T = @TypeOf(value);
    const tyId = try self.typeIdFromNative(T);

    return self.globalFromBytes(tyId, @as([*]const u8, @ptrCast(&value))[0..@sizeOf(T)]);
}

pub fn getGlobal(self: *Builder, id: Core.GlobalId) !Core.Global {
    if (id >= self.global_list.items.len) {
        return error.InvalidGlobal;
    }

    return self.global_list.items[id];
}

pub fn function(self: *Builder, tyId: Core.TypeId) !*FunctionBuilder {
    const index = self.function_list.items.len;

    if (index >= Core.MAX_FUNCTIONS) {
        return error.TooManyFunctions;
    }

    const builder = try FunctionBuilder.init(self, @truncate(index), tyId);

    try self.function_list.append(self.allocator, builder);

    return builder;
}

pub fn getFunction(self: *Builder, id: Core.FunctionId) !*FunctionBuilder {
    if (id >= self.function_list.items.len) {
        return error.InvalidFunction;
    }

    return self.function_list.items[id];
}

/// Calls `allocator.dupe` on the input locals
pub fn foreignFunction(self: *Builder, tyId: Core.TypeId, locals: []Core.TypeId) !Core.ForeignId {
    const dupeLocals = try self.allocator.dupe(Core.TypeId, locals);
    errdefer self.allocator.free(dupeLocals);

    return self.foreignFunctionPreallocated(tyId, dupeLocals);
}

/// Does not call `allocator.dupe` on the input locals
pub fn foreignFunctionPreallocated(self: *Builder, tyId: Core.TypeId, locals: []Core.TypeId) !Core.ForeignId {
    const index = self.foreign_function_list.items.len;

    if (index >= Core.MAX_FUNCTIONS) {
        return error.TooManyForeignFunctions;
    }

    const foreign = Core.ForeignFunction {
        .id = @truncate(index),
        .type = tyId,
        .locals = locals,
    };

    try self.foreign_function_list.append(self.allocator, foreign);

    return foreign.id;
}

pub fn getForeignFunction(self: *Builder, id: Core.ForeignId) !Core.ForeignFunction {
    if (id >= self.foreign_function_list.items.len) {
        return error.InvalidForeignFunction;
    }

    return self.foreign_function_list.items[id];
}

pub fn evidence(self: *Builder, tyId: Core.TypeId) !Core.EvidenceId {
    const index = self.evidence_list.items.len;

    if (index >= Core.MAX_EVIDENCE) {
        return error.TooManyEvidence;
    }

    try self.evidence_list.append(self.allocator, tyId);

    return @truncate(index);
}

pub fn getEvidence(self: *Builder, id: Core.EvidenceId) !Core.TypeId {
    if (id >= self.evidence_list.items.len) {
        return error.InvalidEvidence;
    }

    return self.evidence_list.items[id];
}
