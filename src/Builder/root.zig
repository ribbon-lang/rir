const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;

const Core = @import("Core");


pub const BlockBuilder = @import("BlockBuilder.zig");
pub const FunctionBuilder = @import("FunctionBuilder.zig");
pub const HandlerSetBuilder = @import("HandlerSetBuilder.zig");


const Builder = @This();


allocator: std.mem.Allocator,
type_map: Core.Type.Map = .{},
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
pub inline fn typeId(self: *Builder, ty: Core.Type) !Core.TypeId {
    return self.type_map.typeId(self.allocator, ty);
}

/// Does not call `Core.Type.clone` on the input
pub inline fn typeIdPreallocated(self: *Builder, ty: Core.Type) !Core.TypeId {
    return self.type_map.typeIdPreallocated(self.allocator, ty);
}

pub inline fn typeFromNative(self: *const Builder, comptime T: type) !Core.Type {
    return self.type_map.typeFromNative(T, self.allocator);
}

pub inline fn typeIdFromNative(self: *Builder, comptime T: type) !Core.TypeId {
    return self.type_map.typeIdFromNative(T, self.allocator);
}

pub fn getType(self: *Builder, id: Core.TypeId) !Core.Type {
    return self.type_map.getType(id);
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
