const std = @import("std");
const Core = @import("Core");

const Builder = @import("root.zig");
const FunctionBuilder = Builder.FunctionBuilder;

const BlockBuilder = @This();


function: *FunctionBuilder,
parent: ?*BlockBuilder,
id: Core.BlockId,
has_exit: bool = false,
instructions: std.ArrayListUnmanaged(Core.Instruction) = .{},


pub fn init(function: *FunctionBuilder, parent: ?*BlockBuilder, id: Core.BlockId) !*BlockBuilder {
    const ptr = try function.root.allocator.create(BlockBuilder);

    ptr.* = BlockBuilder {
        .function = function,
        .parent = parent,
        .id = id,
    };

    return ptr;
}

inline fn bCast(b: anytype) u8 {
    return switch (@typeInfo(@TypeOf(b))) {
        .comptime_int => @as(u8, b),
        .int => |info|
            if (info.bits <= 8) switch (info.signedness) {
                .unsigned => @as(u8, b),
                .signed => @as(u8, @as(std.meta.Int(.unsigned, info.bits), @bitCast(b))),
            }
            else @bitCast(@as(std.meta.Int(info.signedness, 8), @intCast(b))),
        .@"enum" => bCast(@intFromEnum(b)),
        else => @as(u8, @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(b))), @bitCast(b))),
    };
}

inline fn sCast(b: anytype) u16 {
    return switch (@typeInfo(@TypeOf(b))) {
        .comptime_int => @as(u16, b),
        .int => |info|
            if (info.bits <= 16) switch (info.signedness) {
                .unsigned => @as(u16, b),
                .signed => @as(u16, @as(std.meta.Int(.unsigned, info.bits), @bitCast(b))),
            }
            else @bitCast(@as(std.meta.Int(info.signedness, 16), @intCast(b))),
        .@"enum" => bCast(@intFromEnum(b)),
        else => @as(u16, @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(b))), @bitCast(b))),
    };
}

inline fn iCast(b: anytype) u32 {
    return switch (@typeInfo(@TypeOf(b))) {
        .comptime_int => @as(u32, b),
        .int => |info|
            if (info.bits <= 32) switch (info.signedness) {
                .unsigned => @as(u32, b),
                .signed => @as(u32, @as(std.meta.Int(.unsigned, info.bits), @bitCast(b))),
            }
            else @bitCast(@as(std.meta.Int(info.signedness, 32), @intCast(b))),
        .@"enum" => bCast(@intFromEnum(b)),
        else => @as(u32, @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(b))), @bitCast(b))),
    };
}

inline fn wCast(b: anytype) u64 {
    return switch (@typeInfo(@TypeOf(b))) {
        .comptime_int => @as(u64, b),
        .int => |info|
            if (info.bits <= 64) switch (info.signedness) {
                .unsigned => @as(u64, b),
                .signed => @as(u64, @as(std.meta.Int(.unsigned, info.bits), @bitCast(b))),
            }
            else @bitCast(@as(std.meta.Int(info.signedness, 64), @intCast(b))),
        .@"enum" => bCast(@intFromEnum(b)),
        else => @as(u64, @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(b))), @bitCast(b))),
    };
}


inline fn wideImmediate(self: *BlockBuilder, x: u64) !void {
    try @call(.always_inline, std.ArrayListUnmanaged(Core.Instruction).append, .{
        &self.instructions,
        self.function.root.allocator,
        @as(Core.Instruction, @bitCast(wCast(x))),
    });
}

inline fn op(self: *BlockBuilder, comptime code: Core.Op.Code, data: anytype) !void {
    try @call(.always_inline, std.ArrayListUnmanaged(Core.Instruction).append, .{
        &self.instructions,
        self.function.root.allocator,
        Core.Instruction {
            .code = code,
            .data = @unionInit(Core.Op.Data, @tagName(code), data),
        },
    });
}

inline fn exitOp(self: *BlockBuilder, comptime code: Core.Op.Code, data: anytype) !void {
    if (self.has_exit) {
        return error.MultipleExits;
    }
    try self.op(code, data);
    self.has_exit = true;
}


pub fn nop(self: *BlockBuilder) !void {
    try self.op(.nop, {});
}

pub fn halt(self: *BlockBuilder) !void {
    try self.exitOp(.halt, {});
}

pub fn trap(self: *BlockBuilder) !void {
    try self.exitOp(.trap, {});
}

pub fn block(self: *BlockBuilder) !void {
    try self.op(.block, {});
}

pub fn with(self: *BlockBuilder) !void {
    try self.op(.with, {});
}

pub fn @"if"(self: *BlockBuilder, x: Core.Op.ZeroCheck) !void {
    try self.op(.@"if", x);
}

pub fn when(self: *BlockBuilder, x: Core.Op.ZeroCheck) !void {
    try self.op(.when, x);
}

pub fn re(self: *BlockBuilder, x: Core.Op.OptZeroCheck) !void {
    if (x != .none) {
        try self.op(.re, x);
    } else {
        try self.exitOp(.re, x);
    }
}

pub fn br(self: *BlockBuilder, x: Core.Op.OptZeroCheck) !void {
    if (x != .none) {
        try self.op(.br, x);
    } else {
        try self.exitOp(.br, x);
    }
}

pub fn call(self: *BlockBuilder) !void {
    try self.op(.call, {});
}

pub fn prompt(self: *BlockBuilder) !void {
    try self.op(.prompt, {});
}

pub fn ret(self: *BlockBuilder) !void {
    try self.exitOp(.ret, {});
}

pub fn term(self: *BlockBuilder) !void {
    try self.exitOp(.term, {});
}

pub fn alloca(self: *BlockBuilder, x: Core.RegisterLocalOffset) !void {
    try self.op(.alloca, x);
}

pub fn addr(self: *BlockBuilder) !void {
    try self.op(.addr, {});
}

pub fn read(self: *BlockBuilder) !void {
    try self.op(.read, {});
}

pub fn write(self: *BlockBuilder) !void {
    try self.op(.write, {});
}

pub fn load(self: *BlockBuilder) !void {
    try self.op(.load, {});
}

pub fn store(self: *BlockBuilder) !void {
    try self.op(.store, {});
}

pub fn clear(self: *BlockBuilder) !void {
    try self.op(.clear, {});
}

pub fn swap(self: *BlockBuilder) !void {
    try self.op(.swap, {});
}

pub fn copy(self: *BlockBuilder) !void {
    try self.op(.copy, {});
}

pub fn add(self: *BlockBuilder) !void {
    try self.op(.add, {});
}

pub fn sub(self: *BlockBuilder) !void {
    try self.op(.sub, {});
}

pub fn mul(self: *BlockBuilder) !void {
    try self.op(.mul, {});
}

pub fn div(self: *BlockBuilder) !void {
    try self.op(.div, {});
}

pub fn rem(self: *BlockBuilder) !void {
    try self.op(.rem, {});
}

pub fn neg(self: *BlockBuilder) !void {
    try self.op(.neg, {});
}

pub fn band(self: *BlockBuilder) !void {
    try self.op(.band, {});
}

pub fn bor(self: *BlockBuilder) !void {
    try self.op(.bor, {});
}

pub fn bxor(self: *BlockBuilder) !void {
    try self.op(.bxor, {});
}

pub fn bnot(self: *BlockBuilder) !void {
    try self.op(.bnot, {});
}

pub fn bshiftl(self: *BlockBuilder) !void {
    try self.op(.bshiftl, {});
}

pub fn bshiftr(self: *BlockBuilder) !void {
    try self.op(.bshiftr, {});
}

pub fn eq(self: *BlockBuilder) !void {
    try self.op(.eq, {});
}

pub fn ne(self: *BlockBuilder) !void {
    try self.op(.ne, {});
}

pub fn lt(self: *BlockBuilder) !void {
    try self.op(.lt, {});
}

pub fn gt(self: *BlockBuilder) !void {
    try self.op(.gt, {});
}

pub fn le(self: *BlockBuilder) !void {
    try self.op(.le, {});
}

pub fn ge(self: *BlockBuilder) !void {
    try self.op(.ge, {});
}

pub fn ext(self: *BlockBuilder, x: Core.BitSize) !void {
    try self.op(.ext, x);
}

pub fn trunc(self: *BlockBuilder, x: Core.BitSize) !void {
    try self.op(.trunc, x);
}

pub fn cast(self: *BlockBuilder, x: Core.TypeId) !void {
    try self.op(.cast, x);
}


pub fn new_local(self: *BlockBuilder, x: Core.TypeId) !void {
    try self.op(.new_local, x);
}

pub fn ref_local(self: *BlockBuilder, x: Core.LocalId) !void {
    try self.op(.ref_local, x);
}

pub fn ref_block(self: *BlockBuilder, x: Core.BlockId) !void {
    try self.op(.ref_block, x);
}

pub fn ref_function(self: *BlockBuilder, x: Core.FunctionId) !void {
    try self.op(.ref_function, x);
}

pub fn ref_global(self: *BlockBuilder, x: Core.GlobalId) !void {
    try self.op(.ref_global, x);
}

pub fn ref_upvalue(self: *BlockBuilder, x: Core.UpvalueId) !void {
    try self.op(.ref_upvalue, x);
}


pub fn discard(self: *BlockBuilder) !void {
    try self.op(.discard, {});
}


pub fn im_b(self: *BlockBuilder, x: anytype) !void {
    const ty = try self.function.root.typeIdFromNative(@TypeOf(x));
    try self.op(.im_b, .{.type = ty, .data = bCast(x)});
}

pub fn im_s(self: *BlockBuilder, x: anytype) !void {
    const ty = try self.function.root.typeIdFromNative(@TypeOf(x));
    try self.op(.im_s, .{.type = ty, .data = sCast(x)});
}

pub fn im_i(self: *BlockBuilder, x: anytype) !void {
    const ty = try self.function.root.typeIdFromNative(@TypeOf(x));
    try self.op(.im_i, .{.type = ty, .data = iCast(x)});
}

pub fn im_w(self: *BlockBuilder, x: anytype) !void {
    const ty = try self.function.root.typeIdFromNative(@TypeOf(x));
    try self.op(.im_w, .{.type = ty});
    try self.wideImmediate(x);
}


comptime {
    for (std.meta.fieldNames(Core.Op.Code)) |opName| {
        if (!@hasDecl(BlockBuilder, opName)) {
            @compileError("missing BlockBuilder method: `" ++ opName ++ "`");
        }
    }
}
