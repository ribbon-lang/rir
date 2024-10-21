const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

const IR = @import("../root.zig");

const RbcBackend = @This();


allocator: std.mem.Allocator,

type_map: *IR.Type.Map,
foreign_function_list: *IR.Function.ForeignList,

builder: RbcBuilder,

global_types: std.ArrayListUnmanaged(IR.TypeId) = .{},
global_lookup: std.ArrayHashMapUnmanaged(Ref(IR.GlobalId), RbcCore.GlobalIndex, MiscUtils.SimpleHashContext, false) = .{},

function_types: std.ArrayListUnmanaged(IR.TypeId) = .{},
function_lookup: std.ArrayHashMapUnmanaged(Ref(IR.FunctionId), RbcCore.FunctionIndex, MiscUtils.SimpleHashContext, false) = .{},


pub fn Ref (comptime T: type) type {
    return packed struct {
        module: IR.ModuleId,
        id: T,
    };
}


/// Allocator provided should be an arena or a similar allocator,
/// that does not care about freeing individual allocations
pub fn init(allocator: std.mem.Allocator, typeMap: *IR.Type.Map, foreignFunctionList: *IR.Function.ForeignList) !RbcBackend {
    return RbcBackend {
        .allocator = allocator,
        .type_map = typeMap,
        .foreign_function_list = foreignFunctionList,
        .builder = try RbcBuilder.init(allocator),
    };
}

/// Calls `Type.clone` on the input, if the type is not found in the map
pub inline fn typeId(self: *IR, ty: IR.Type) !IR.TypeId {
    return self.type_map.typeId(ty);
}

/// Does not call `Type.clone` on the input
pub inline fn typeIdPreallocated(self: *IR, ty: IR.Type) !IR.TypeId {
    return self.type_map.typeIdPreallocated(ty);
}

pub inline fn typeFromNative(self: *const IR, comptime T: type) !IR.Type {
    return self.type_map.typeFromNative(T);
}

pub inline fn typeIdFromNative(self: *IR, comptime T: type) !IR.TypeId {
    return self.type_map.typeIdFromNative(T);
}

pub inline fn getType(self: *IR, id: IR.TypeId) !IR.Type {
    return self.type_map.getType(id);
}

/// Calls `allocator.dupe` on the input locals
pub inline fn foreign(self: *RbcBackend, tyId: IR.TypeId, locals: []IR.TypeId) !IR.ForeignId {
    return self.foreign_function_list.foreign(tyId, locals);
}

/// Does not call `allocator.dupe` on the input locals
pub inline fn foreignPreallocated(self: *RbcBackend, tyId: IR.TypeId, locals: []IR.TypeId) !IR.ForeignId {
    return self.foreign_function_list.foreignPreallocated(tyId, locals);
}

pub inline fn getForeign(self: *RbcBackend, id: IR.ForeignId) !IR.Function.Foreign {
    return self.foreign_function_list.getForeign(id);
}
