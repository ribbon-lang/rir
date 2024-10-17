const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");


pub const Op = @import("Op.zig");
pub const Operand = @import("Operand.zig").Operand;
pub const Type = @import("Type.zig").Type;


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
