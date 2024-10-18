const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

const IR = @This();

pub const Block = @import("Block.zig");
pub const Function = @import("Function.zig");
pub const HandlerSet = @import("HandlerSet.zig");
pub const Op = @import("Op.zig");
pub const Operand = @import("Operand.zig").Operand;
pub const Type = @import("Type.zig").Type;


allocator: std.mem.Allocator,
type_map: IR.Type.Map = .{},
global_list: std.ArrayListUnmanaged(IR.Global) = .{},
function_list: std.ArrayListUnmanaged(*Function) = .{},
foreign_function_list: std.ArrayListUnmanaged(IR.ForeignFunction) = .{},
evidence_list: std.ArrayListUnmanaged(IR.TypeId) = .{},


pub const MAX_TYPES = std.math.maxInt(TypeId);
pub const MAX_GLOBALS = std.math.maxInt(GlobalId);
pub const MAX_FUNCTIONS = std.math.maxInt(FunctionId);
pub const MAX_HANDLER_SETS = std.math.maxInt(HandlerSetId);
pub const MAX_EVIDENCE = RbcCore.MAX_EVIDENCE;
pub const MAX_BLOCKS = RbcCore.MAX_BLOCKS;
pub const MAX_REGISTERS = RbcCore.MAX_REGISTERS;
pub const MAX_LOCALS = std.math.maxInt(LocalId);


pub const RegisterId = RbcCore.RegisterIndex;
pub const HandlerSetId = RbcCore.HandlerSetIndex;
pub const EvidenceId = RbcCore.EvidenceIndex;
pub const TypeId = RbcCore.Info.TypeIndex;
pub const BlockId = RbcCore.BlockIndex;
pub const FunctionId = RbcCore.FunctionIndex;
pub const ForeignId = RbcCore.ForeignId;
pub const GlobalId = RbcCore.GlobalIndex;
pub const UpvalueId = RbcCore.UpvalueIndex;
pub const LocalId = u16;

pub const BitSize = enum(u2) { b8, b16, b32, b64 };

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

pub const ForeignFunction = struct {
    id: ForeignId,
    type: TypeId,
    locals: []TypeId,
};


/// Allocator provided should be an arena or a similar allocator,
/// that does not care about freeing individual allocations
pub fn init(allocator: std.mem.Allocator) IR {
    return IR {
        .allocator = allocator,
    };
}

/// Calls `IR.Type.clone` on the input, if the type is not found in the map
pub inline fn typeId(self: *IR, ty: IR.Type) !IR.TypeId {
    return self.type_map.typeId(self.allocator, ty);
}

/// Does not call `IR.Type.clone` on the input
pub inline fn typeIdPreallocated(self: *IR, ty: IR.Type) !IR.TypeId {
    return self.type_map.typeIdPreallocated(self.allocator, ty);
}

pub inline fn typeFromNative(self: *const IR, comptime T: type) !IR.Type {
    return self.type_map.typeFromNative(T, self.allocator);
}

pub inline fn typeIdFromNative(self: *IR, comptime T: type) !IR.TypeId {
    return self.type_map.typeIdFromNative(T, self.allocator);
}

pub fn getType(self: *IR, id: IR.TypeId) !IR.Type {
    return self.type_map.getType(id);
}

/// Calls `allocator.dupe` on the input bytes
pub fn globalFromBytes(self: *IR, tyId: IR.TypeId, bytes: []const u8) !IR.GlobalId {
    const dupeBytes = try self.allocator.dupe(u8, bytes);
    errdefer self.allocator.free(dupeBytes);

    return self.globalFromBytesPreallocated(tyId, dupeBytes);
}

/// Does not call `allocator.dupe` on the input bytes
pub fn globalFromBytesPreallocated(self: *IR, tyId: IR.TypeId, bytes: []u8) !IR.GlobalId {
    const index = self.global_list.items.len;

    if (index >= IR.MAX_GLOBALS) {
        return error.TooManyGlobals;
    }

    const global = IR.Global {
        .id = @truncate(index),
        .type = tyId,
        .value = bytes,
    };

    try self.global_list.append(self.allocator, global);

    return global.id;
}

pub fn globalFromNative(self: *IR, value: anytype) !IR.GlobalId {
    const T = @TypeOf(value);
    const tyId = try self.typeIdFromNative(T);

    return self.globalFromBytes(tyId, @as([*]const u8, @ptrCast(&value))[0..@sizeOf(T)]);
}

pub fn getGlobal(self: *IR, id: IR.GlobalId) !IR.Global {
    if (id >= self.global_list.items.len) {
        return error.InvalidGlobal;
    }

    return self.global_list.items[id];
}

pub fn function(self: *IR, tyId: IR.TypeId) !*Function {
    const index = self.function_list.items.len;

    if (index >= IR.MAX_FUNCTIONS) {
        return error.TooManyFunctions;
    }

    const builder = try Function.init(self, @truncate(index), tyId);

    try self.function_list.append(self.allocator, builder);

    return builder;
}

pub fn getFunction(self: *IR, id: IR.FunctionId) !*Function {
    if (id >= self.function_list.items.len) {
        return error.InvalidFunction;
    }

    return self.function_list.items[id];
}

/// Calls `allocator.dupe` on the input locals
pub fn foreignFunction(self: *IR, tyId: IR.TypeId, locals: []IR.TypeId) !IR.ForeignId {
    const dupeLocals = try self.allocator.dupe(IR.TypeId, locals);
    errdefer self.allocator.free(dupeLocals);

    return self.foreignFunctionPreallocated(tyId, dupeLocals);
}

/// Does not call `allocator.dupe` on the input locals
pub fn foreignFunctionPreallocated(self: *IR, tyId: IR.TypeId, locals: []IR.TypeId) !IR.ForeignId {
    const index = self.foreign_function_list.items.len;

    if (index >= IR.MAX_FUNCTIONS) {
        return error.TooManyForeignFunctions;
    }

    const foreign = IR.ForeignFunction {
        .id = @truncate(index),
        .type = tyId,
        .locals = locals,
    };

    try self.foreign_function_list.append(self.allocator, foreign);

    return foreign.id;
}

pub fn getForeignFunction(self: *IR, id: IR.ForeignId) !IR.ForeignFunction {
    if (id >= self.foreign_function_list.items.len) {
        return error.InvalidForeignFunction;
    }

    return self.foreign_function_list.items[id];
}

pub fn evidence(self: *IR, tyId: IR.TypeId) !IR.EvidenceId {
    const index = self.evidence_list.items.len;

    if (index >= IR.MAX_EVIDENCE) {
        return error.TooManyEvidence;
    }

    try self.evidence_list.append(self.allocator, tyId);

    return @truncate(index);
}

pub fn getEvidence(self: *IR, id: IR.EvidenceId) !IR.TypeId {
    if (id >= self.evidence_list.items.len) {
        return error.InvalidEvidence;
    }

    return self.evidence_list.items[id];
}
