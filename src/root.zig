const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

const IR = @This();

pub const Backend = @import("Backend/root.zig");
pub const Block = @import("Block.zig");
pub const Function = @import("Function.zig");
pub const HandlerSet = @import("HandlerSet.zig");
pub const Op = @import("Op.zig");
pub const Operand = @import("Operand.zig").Operand;
pub const Type = @import("Type.zig").Type;


allocator: std.mem.Allocator,
id: ModuleId,
type_map: *Type.Map,
foreign_function_list: *Function.ForeignList,
global_list: std.ArrayListUnmanaged(Global) = .{},
function_list: std.ArrayListUnmanaged(*Function) = .{},
evidence_list: std.ArrayListUnmanaged(TypeId) = .{},


pub const MAX_TYPES = std.math.maxInt(TypeId);
pub const MAX_GLOBALS = std.math.maxInt(GlobalId);
pub const MAX_FUNCTIONS = std.math.maxInt(FunctionId);
pub const MAX_HANDLER_SETS = std.math.maxInt(HandlerSetId);
pub const MAX_EVIDENCE = RbcCore.MAX_EVIDENCE;
pub const MAX_BLOCKS = RbcCore.MAX_BLOCKS;
pub const MAX_REGISTERS = RbcCore.MAX_REGISTERS;
pub const MAX_LOCALS = std.math.maxInt(LocalId);


pub const ModuleId = u16;
pub const RegisterId = RbcCore.RegisterIndex;
pub const RegisterOffset = RbcCore.RegisterLocalOffset;
pub const HandlerSetId = RbcCore.HandlerSetIndex;
pub const EvidenceId = RbcCore.EvidenceIndex;
pub const TypeId = RbcCore.Info.TypeIndex;
pub const BlockId = RbcCore.BlockIndex;
pub const FunctionId = RbcCore.FunctionIndex;
pub const ForeignId = RbcCore.ForeignId;
pub const GlobalId = RbcCore.GlobalIndex;
pub const UpvalueId = RbcCore.UpvalueIndex;
pub const LocalId = u16;

pub const Instruction = packed struct {
    code: Op.Code,
    data: Op.Data,

    comptime {
        if (@sizeOf(Instruction) != 8) {
            @compileError(std.fmt.comptimePrint("Instruction size changed: {}", .{@sizeOf(Instruction)}));
        }
    }
};

pub const Global = struct {
    id: GlobalId,
    type: TypeId,
    value: []u8,
};


/// Allocator provided should be an arena or a similar allocator,
/// that does not care about freeing individual allocations
pub fn init(allocator: std.mem.Allocator, id: ModuleId, typeMap: *Type.Map, foreignFunctionList: *Function.ForeignList) IR {
    return IR {
        .allocator = allocator,
        .id = id,
        .type_map = typeMap,
        .foreign_function_list = foreignFunctionList,
    };
}

/// Calls `Type.clone` on the input, if the type is not found in the map
pub inline fn typeId(self: *IR, ty: Type) !TypeId {
    return self.type_map.typeId(ty);
}

/// Does not call `Type.clone` on the input
pub inline fn typeIdPreallocated(self: *IR, ty: Type) !TypeId {
    return self.type_map.typeIdPreallocated(ty);
}

pub inline fn typeFromNative(self: *const IR, comptime T: type) !Type {
    return self.type_map.typeFromNative(T);
}

pub inline fn typeIdFromNative(self: *IR, comptime T: type) !TypeId {
    return self.type_map.typeIdFromNative(T);
}

pub inline fn getType(self: *IR, id: TypeId) !Type {
    return self.type_map.getType(id);
}

/// Calls `allocator.dupe` on the input locals
pub inline fn foreign(self: *IR, tyId: TypeId, locals: []TypeId) !ForeignId {
    return self.foreign_function_list.foreign(tyId, locals);
}

/// Does not call `allocator.dupe` on the input locals
pub inline fn foreignPreallocated(self: *IR, tyId: TypeId, locals: []TypeId) !ForeignId {
    return self.foreign_function_list.foreignPreallocated(tyId, locals);
}

pub inline fn getForeign(self: *IR, id: ForeignId) !Function.Foreign {
    return self.foreign_function_list.getForeign(id);
}

/// Calls `allocator.dupe` on the input bytes
pub fn globalFromBytes(self: *IR, tyId: TypeId, bytes: []const u8) !GlobalId {
    const dupeBytes = try self.allocator.dupe(u8, bytes);
    errdefer self.allocator.free(dupeBytes);

    return self.globalFromBytesPreallocated(tyId, dupeBytes);
}

/// Does not call `allocator.dupe` on the input bytes
pub fn globalFromBytesPreallocated(self: *IR, tyId: TypeId, bytes: []u8) !GlobalId {
    const index = self.global_list.items.len;

    if (index >= MAX_GLOBALS) {
        return error.TooManyGlobals;
    }

    const global = Global {
        .id = @truncate(index),
        .type = tyId,
        .value = bytes,
    };

    try self.global_list.append(self.allocator, global);

    return global.id;
}

pub fn globalFromNative(self: *IR, value: anytype) !GlobalId {
    const T = @TypeOf(value);
    const tyId = try self.typeIdFromNative(T);

    return self.globalFromBytes(tyId, @as([*]const u8, @ptrCast(&value))[0..@sizeOf(T)]);
}

pub fn getGlobal(self: *IR, id: GlobalId) !Global {
    if (id >= self.global_list.items.len) {
        return error.InvalidGlobal;
    }

    return self.global_list.items[id];
}

pub fn function(self: *IR, tyId: TypeId) !*Function {
    const index = self.function_list.items.len;

    if (index >= MAX_FUNCTIONS) {
        return error.TooManyFunctions;
    }

    const builder = try Function.init(self, @truncate(index), tyId);

    try self.function_list.append(self.allocator, builder);

    return builder;
}

pub fn getFunction(self: *IR, id: FunctionId) !*Function {
    if (id >= self.function_list.items.len) {
        return error.InvalidFunction;
    }

    return self.function_list.items[id];
}

pub fn evidence(self: *IR, tyId: TypeId) !EvidenceId {
    const index = self.evidence_list.items.len;

    if (index >= MAX_EVIDENCE) {
        return error.TooManyEvidence;
    }

    try self.evidence_list.append(self.allocator, tyId);

    return @truncate(index);
}

pub fn getEvidence(self: *IR, id: EvidenceId) !TypeId {
    if (id >= self.evidence_list.items.len) {
        return error.InvalidEvidence;
    }

    return self.evidence_list.items[id];
}


test {
    std.testing.refAllDeclsRecursive(IR);
}
