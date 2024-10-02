const std = @import("std");
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

pub const RegisterId = RbcCore.RegisterIndex;
pub const TypeId = RbcCore.Info.TypeIndex;
pub const BlockId = RbcCore.BlockIndex;
pub const FunctionId = RbcCore.FunctionIndex;
pub const GlobalId = RbcCore.GlobalIndex;
pub const UpvalueId = RbcCore.UpvalueIndex;
pub const LocalId = u16;
pub const VariableWidth = void;

pub const OpCode = enum(u8) {
    // ISA instructions:
    nop,
    halt, trap, block, with, @"if", when, re, br, call, prompt, ret, term,
    alloca, addr, read, write, load, store, clear, swap, copy,
    add, sub, mul, div, rem, neg,
    band, bor, bxor, bnot, bshiftl, bshiftr,
    eq, ne, lt, gt, le, ge,
    ext, trunc, cast,

    // IR-specific instructions:
    new_local,
    ref_local,
    ref_block,
    ref_function,
    ref_global,
    ref_upvalue,

    imm, phi,
};

comptime {
    for (ISA.Instructions) |category| {
        for (category.kinds) |kind| {
            const name = kind.humanFriendlyName();

            if (!@hasField(OpCode, name)) {
                @compileError("missing OpCode: `" ++ name ++ "`");
            }
        }
    }
}

pub const ZeroCheck = enum(u1) { zero, non_zero };
pub const OptZeroCheck = enum(u2) { none, zero, non_zero };
pub const BitSize = enum(u2) { b8, b16, b32, b64 };

pub const OpData = packed union {
    nop: void,
    halt: void, trap: void, block: void, with: void, @"if": ZeroCheck, when: ZeroCheck, re: OptZeroCheck, br: OptZeroCheck, call: void, prompt: void, ret: void, term: void,
    alloca: RbcCore.RegisterLocalOffset, addr: void, read: void, write: void, load: void, store: void, clear: void, swap: void, copy: void,
    add: void, sub: void, mul: void, div: void, rem: void, neg: void,
    band: void, bor: void, bxor: void, bnot: void, bshiftl: void, bshiftr: void,
    eq: void, ne: void, lt: void, gt: void, le: void, ge: void,
    ext: BitSize, trunc: BitSize, cast: TypeId,

    new_local: TypeId,
    ref_local: LocalId,
    ref_block: BlockId,
    ref_function: FunctionId,
    ref_global: GlobalId,
    ref_upvalue: UpvalueId,

    imm: VariableWidth,
    phi: VariableWidth,
};

pub const Instruction = packed struct {
    code: OpCode,
    data: OpData,
};

pub const Operand = union(enum) {
    intermediate: Intermediate,
    immediate: Immediate,
    block: BlockId,
    function: FunctionId,
    global: GlobalId,
    upvalue: UpvalueId,
    local: LocalId,

    pub const Intermediate = packed struct {
        type: TypeId,
        register: RegisterId,
    };

    pub const Immediate = packed struct {
        type: TypeId,
        value: u64,
    };
};


test {
    std.testing.refAllDeclsRecursive(@This());
}
