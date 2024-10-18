const std = @import("std");
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");

const IR = @import("root.zig");


pub const ZeroCheck = enum(u1) { zero, non_zero };
pub const OptZeroCheck = enum(u2) { none, zero, non_zero };

pub const Code = enum(u8) {
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

    discard,

    im_b, im_s, im_i, im_w,

    comptime {
        for (std.meta.fieldNames(Data)) |opName| {
            if (!@hasField(Code, opName)) {
                @compileError("missing OpCode: `" ++ opName ++ "`");
            }
        }
    }
};

comptime {
    for (ISA.Instructions) |category| {
        for (category.kinds) |kind| {
            const name = kind.humanFriendlyName();

            if (!@hasField(Code, name)) {
                @compileError("missing OpCode: `" ++ name ++ "`");
            }
        }
    }
}

pub const Data = packed union {
    nop: void,
    halt: void, trap: void, block: void, with: void, @"if": ZeroCheck, when: ZeroCheck, re: OptZeroCheck, br: OptZeroCheck, call: void, prompt: void, ret: void, term: void,
    alloca: RbcCore.RegisterLocalOffset, addr: void, read: void, write: void, load: void, store: void, clear: void, swap: void, copy: void,
    add: void, sub: void, mul: void, div: void, rem: void, neg: void,
    band: void, bor: void, bxor: void, bnot: void, bshiftl: void, bshiftr: void,
    eq: void, ne: void, lt: void, gt: void, le: void, ge: void,
    ext: IR.BitSize, trunc: IR.BitSize, cast: IR.TypeId,

    new_local: IR.TypeId,
    ref_local: IR.LocalId,
    ref_block: IR.BlockId,
    ref_function: IR.FunctionId,
    ref_global: IR.GlobalId,
    ref_upvalue: IR.UpvalueId,

    discard: void,

    im_b: Immediate(u8),
    im_s: Immediate(u16),
    im_i: Immediate(u32),
    im_w: IR.TypeId,

    pub fn Immediate (comptime T: type) type {
        return packed struct {
            type: IR.TypeId,
            data: T,
        };
    }

    comptime {
        for (std.meta.fieldNames(Code)) |opName| {
            if (!@hasField(Data, opName)) {
                @compileError("missing OpData: `" ++ opName ++ "`");
            }
        }
    }
};
