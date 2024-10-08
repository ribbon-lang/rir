const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");

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

pub const MAX_TYPES = std.math.maxInt(TypeId);
pub const MAX_GLOBALS = std.math.maxInt(GlobalId);
pub const MAX_FUNCTIONS = std.math.maxInt(FunctionId);
pub const MAX_HANDLER_SETS = std.math.maxInt(HandlerSetId);
pub const MAX_EVIDENCE = RbcCore.MAX_EVIDENCE;
pub const MAX_BLOCKS = RbcCore.MAX_BLOCKS;
pub const MAX_REGISTERS = RbcCore.MAX_REGISTERS;
pub const MAX_LOCALS = std.math.maxInt(LocalId);

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

    discard,

    im_b, im_s, im_i, im_w,

    comptime {
        for (std.meta.fieldNames(OpData)) |opName| {
            if (!@hasField(OpCode, opName)) {
                @compileError("missing OpCode: `" ++ opName ++ "`");
            }
        }
    }
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

    discard: void,

    im_b: Immediate(u8),
    im_s: Immediate(u16),
    im_i: Immediate(u32),
    im_w: TypeId,

    pub fn Immediate (comptime T: type) type {
        return packed struct {
            type: TypeId,
            data: T,
        };
    }

    comptime {
        for (std.meta.fieldNames(OpCode)) |opName| {
            if (!@hasField(OpData, opName)) {
                @compileError("missing OpData: `" ++ opName ++ "`");
            }
        }
    }
};

pub const Instruction = packed struct {
    code: OpCode,
    data: OpData,

    comptime {
        if (@sizeOf(Instruction) != 8) {
            @compileError(std.fmt.comptimePrint("Instruction size changed: {}", .{@sizeOf(Instruction)}));
        }
    }
};

pub const Operand = union(enum) {
    intermediate: Intermediate,
    immediate: Immediate,
    block: BlockId,
    function: FunctionId,
    global: GlobalId,
    upvalue: UpvalueId,
    handler_set: HandlerSetId,
    evidence: EvidenceId,
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

pub const Type = union(enum) {
    void: void,
    bool: void,
    u8: void, u16: void, u32: void, u64: void,
    s8: void, s16: void, s32: void, s64: void,
    f32: void, f64: void,
    pointer: TypeId,
    array: Type.Array,
    product: []TypeId,
    sum: Sum,
    raw_sum: []TypeId,
    function: Type.Function,

    block: void,
    handler_set: void,
    evidence: Type.Function,

    const BASIC_TYPE_NAMES = [_][:0]const u8 {
        "void",
        "bool",
        "u8", "u16", "u32", "u64",
        "s8", "s16", "s32", "s64",
        "f32", "f64",
        "block",
    };

    pub const BASIC_TYPE_IDS = type_ids: {
        var type_ids = [1]TypeId { undefined } ** BASIC_TYPE_NAMES.len;

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
        discriminator: TypeId,
        types: []TypeId,

        pub fn clone(self: *const Type.Sum, allocator: std.mem.Allocator) !Type.Sum {
            return Type.Sum {
                .discriminator = self.discriminator,
                .types = try allocator.dupe(TypeId, self.types),
            };
        }

        pub fn deinit(self: Type.Sum, allocator: std.mem.Allocator) void {
            allocator.free(self.types);
        }
    };

    pub const Array = struct {
        length: usize,
        element: TypeId,
    };

    pub const Function = struct {
        return_type: TypeId,
        termination_type: TypeId,
        effects: []EvidenceId,
        parameter_types: []TypeId,

        pub fn deinit(self: Type.Function, allocator: std.mem.Allocator) void {
            allocator.free(self.effects);
            allocator.free(self.parameter_types);
        }

        pub fn clone(self: *const Type.Function, allocator: std.mem.Allocator) !Type.Function {
            const effects = try allocator.dupe(EvidenceId, self.effects);
            errdefer allocator.free(effects);

            const parameter_types = try allocator.dupe(TypeId, self.parameter_types);
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
            .product => |field_types| return Type { .product = try allocator.dupe(TypeId, field_types) },
            .sum => |sum| return Type { .sum = try sum.clone(allocator) },
            .raw_sum => |field_types| return Type { .raw_sum = try allocator.dupe(TypeId, field_types) },
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
};

pub const BlockBuilder = struct {
    function: *FunctionBuilder,
    parent: ?*BlockBuilder,
    id: BlockId,
    type: ?TypeId = null,
    instructions: std.ArrayListUnmanaged(Instruction) = .{},
    has_exit: bool = false,

    pub fn init(function: *FunctionBuilder, parent: ?*BlockBuilder, id: BlockId) !*BlockBuilder {
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
        try self.instructions.append(self.function.root.allocator, @bitCast(wCast(x)));
    }

    inline fn op(self: *BlockBuilder, comptime code: OpCode, data: anytype) !void {
        try self.instructions.append(self.function.root.allocator, Instruction { .code = code, .data = @unionInit(OpData, @tagName(code), data) });
    }

    inline fn exitOp(self: *BlockBuilder, comptime code: OpCode, data: anytype) !void {
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

    pub fn @"if"(self: *BlockBuilder, x: ZeroCheck) !void {
        try self.op(.@"if", x);
    }

    pub fn when(self: *BlockBuilder, x: ZeroCheck) !void {
        try self.op(.when, x);
    }

    pub fn re(self: *BlockBuilder, x: OptZeroCheck) !void {
        if (x != .none) {
            try self.op(.re, x);
        } else {
            try self.exitOp(.re, x);
        }
    }

    pub fn br(self: *BlockBuilder, x: OptZeroCheck) !void {
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

    pub fn alloca(self: *BlockBuilder, x: RbcCore.RegisterLocalOffset) !void {
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

    pub fn ext(self: *BlockBuilder, x: BitSize) !void {
        try self.op(.ext, x);
    }

    pub fn trunc(self: *BlockBuilder, x: BitSize) !void {
        try self.op(.trunc, x);
    }

    pub fn cast(self: *BlockBuilder, x: TypeId) !void {
        try self.op(.cast, x);
    }


    pub fn new_local(self: *BlockBuilder, x: TypeId) !void {
        try self.op(.new_local, x);
    }

    pub fn ref_local(self: *BlockBuilder, x: LocalId) !void {
        try self.op(.ref_local, x);
    }

    pub fn ref_block(self: *BlockBuilder, x: BlockId) !void {
        try self.op(.ref_block, x);
    }

    pub fn ref_function(self: *BlockBuilder, x: FunctionId) !void {
        try self.op(.ref_function, x);
    }

    pub fn ref_global(self: *BlockBuilder, x: GlobalId) !void {
        try self.op(.ref_global, x);
    }

    pub fn ref_upvalue(self: *BlockBuilder, x: UpvalueId) !void {
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
        for (std.meta.fieldNames(OpCode)) |opName| {
            if (!@hasDecl(BlockBuilder, opName)) {
                @compileError("missing BlockBuilder method: `" ++ opName ++ "`");
            }
        }
    }
};

pub const FunctionBuilder = struct {
    root: *IrBuilder,
    id: FunctionId,
    type: TypeId,
    blocks: std.ArrayListUnmanaged(*BlockBuilder),
    local_types: std.ArrayListUnmanaged(TypeId),
    parent: ?*FunctionBuilder = null,
    upvalue_indices: std.ArrayListUnmanaged(LocalId) = .{},
    handler_sets: std.ArrayListUnmanaged(*HandlerSetBuilder) = .{},

    pub fn init(root: *IrBuilder, id: FunctionId, tyId: TypeId) !*FunctionBuilder {
        const ptr = try root.allocator.create(FunctionBuilder);

        const ty = try root.getType(tyId);
        if (ty != .function) {
            return error.TypeError;
        }

        var blocks = std.ArrayListUnmanaged(*BlockBuilder){};
        errdefer blocks.deinit(root.allocator);

        const entryBlock = try BlockBuilder.init(ptr, null, 0);

        entryBlock.type = ty.function.return_type;

        try blocks.append(root.allocator, entryBlock);

        var local_types = std.ArrayListUnmanaged(TypeId){};
        errdefer local_types.deinit(root.allocator);

        for (ty.function.parameter_types) |param| {
            try local_types.append(root.allocator, param);
        }

        ptr.* = FunctionBuilder {
            .root = root,
            .id = id,
            .type = tyId,
            .blocks = blocks,
            .local_types = local_types,
        };

        return ptr;
    }

    pub fn local(self: *FunctionBuilder, tyId: TypeId) !LocalId {
        const index = self.local_types.items.len;

        if (index >= MAX_LOCALS) {
            return error.TooManyLocals;
        }

        try self.local_types.append(self.root.allocator, tyId);

        return @truncate(index);
    }

    pub fn getLocalType(self: *const FunctionBuilder, l: LocalId) !TypeId {
        if (l >= self.local_types.items.len) {
            return error.InvalidRegister;
        }

        return self.local_types.items[l];
    }

    pub fn upvalue(self: *FunctionBuilder, parentLocal: LocalId) !UpvalueId {
        if (self.parent) |parent| {
            _ = try parent.getLocalType(parentLocal);

            const index = self.upvalue_indices.items.len;

            if (index >= MAX_LOCALS) {
                return error.TooManyUpvalues;
            }

            try self.upvalue_indices.append(self.root.allocator, parentLocal);

            return @truncate(index);
        } else {
            return error.InvalidUpvalue;
        }
    }

    pub fn getUpvalueType(self: *const FunctionBuilder, u: UpvalueId) !TypeId {
        if (self.parent) |parent| {
            if (u >= self.upvalue_indices.items.len) {
                return error.InvalidRegister;
            }

            return parent.getLocalType(self.upvalue_indices.items[u]);
        } else {
            return error.InvalidUpvalue;
        }
    }

    pub fn entry(self: *FunctionBuilder) !*BlockBuilder {
        return self.blocks.items[0];
    }

    pub fn block(self: *FunctionBuilder, parent: *BlockBuilder) !*BlockBuilder {
        const index = self.blocks.items.len;

        if (index >= MAX_BLOCKS) {
            return error.TooManyBlocks;
        }

        const newBlock = try BlockBuilder.init(self, parent, @truncate(index));

        try self.blocks.append(self.root.allocator, newBlock);

        return newBlock;
    }

    pub fn getBlock(self: *FunctionBuilder, id: BlockId) !*BlockBuilder {
        if (id >= self.blocks.items.len) {
            return error.InvalidBlock;
        }

        return self.blocks.items[id];
    }

    pub fn handlerSet(self: *FunctionBuilder) !*HandlerSetBuilder {
        const index = self.handler_sets.items.len;

        if (index >= MAX_HANDLER_SETS) {
            return error.TooManyHandlerSets;
        }

        const builder = try HandlerSetBuilder.init(self, @truncate(index));

        try self.handler_sets.append(self.root.allocator, builder);

        return builder;
    }

    pub fn getHandlerSet(self: *FunctionBuilder, id: HandlerSetId) !*HandlerSetBuilder {
        if (id >= self.handler_sets.items.len) {
            return error.InvalidHandlerSet;
        }

        return self.handler_sets.items[id];
    }
};

pub const HandlerSetBuilder = struct {
    parent: *FunctionBuilder,
    id: HandlerSetId,
    handlers: HandlerMap = .{},

    pub fn init(parent: *FunctionBuilder, id: HandlerSetId) !*HandlerSetBuilder {
        const ptr = try parent.root.allocator.create(HandlerSetBuilder);

        ptr.* = HandlerSetBuilder {
            .parent = parent,
            .id = id,
        };

        return ptr;
    }

    pub fn handler(self: *HandlerSetBuilder, evId: EvidenceId) !*FunctionBuilder {
        if (self.handlers.contains(evId)) {
            return error.EvidenceOverlap;
        }

        const evTy = try self.parent.root.getEvidence(evId);

        const builder = try self.parent.root.function(evTy);

        builder.parent = self.parent;

        try self.handlers.put(self.parent.root.allocator, evId, builder);

        return builder;
    }

    pub fn getHandler(self: *HandlerSetBuilder, evId: EvidenceId) !*FunctionBuilder {
        return self.handlers.get(evId) orelse error.InvalidEvidence;
    }
};

const HandlerMap = std.ArrayHashMapUnmanaged(EvidenceId, *FunctionBuilder, MiscUtils.SimpleHashContext, false);
const TypeMap = std.ArrayHashMapUnmanaged(Type, void, MiscUtils.SimpleHashContext, true);
const GlobalList = std.ArrayListUnmanaged(Global);
const FunctionList = std.ArrayListUnmanaged(*FunctionBuilder);
const ForeignFunctionList = std.ArrayListUnmanaged(ForeignFunction);
const HandlerSetList = std.ArrayListUnmanaged(*HandlerSetBuilder);
const EvidenceList = std.ArrayListUnmanaged(TypeId);

pub const IrBuilder = struct {
    allocator: std.mem.Allocator,
    type_map: TypeMap = .{},
    global_list: GlobalList = .{},
    function_list: FunctionList = .{},
    foreign_function_list: ForeignFunctionList = .{},
    evidence_list: EvidenceList = .{},

    /// Allocator provided should be an arena or a similar allocator,
    /// that does not care about freeing individual allocations
    pub fn init(allocator: std.mem.Allocator) IrBuilder {
        return IrBuilder {
            .allocator = allocator,
        };
    }

    /// Calls `Type.clone` on the input, if the type is not found in the map
    pub fn typeId(self: *IrBuilder, ty: Type) !TypeId {
        if (self.type_map.getIndex(ty)) |index| {
            return @truncate(index);
        }

        const index = self.type_map.count();

        if (index >= MAX_TYPES) {
            return error.TooManyTypes;
        }

        try self.type_map.put(self.allocator, try ty.clone(self.allocator), {});

        return @truncate(index);
    }

    /// Does not call `Type.clone` on the input
    pub fn typeIdPreallocated(self: *IrBuilder, ty: Type) !TypeId {
        if (self.type_map.getIndex(ty)) |index| {
            return @truncate(index);
        }

        const index = self.type_map.count();

        if (index >= MAX_TYPES) {
            return error.TooManyTypes;
        }

        try self.type_map.put(self.allocator, ty, {});

        return @truncate(index);
    }

    pub fn typeFromNative(self: *const IrBuilder, comptime T: type) !Type {
        switch (T) {
            void => return .void,
            bool => return .bool,
            u8 => return .u8, u16 => return .u16, u32 => return .u32, u64 => return .u64,
            i8 => return .s8, i16 => return .s16, i32 => return .s32, i64 => return .s64,
            f32 => return .f32, f64 => return .f64,

            else => switch (@typeInfo(T)) {
                .pointer => |info| return Type { .pointer = try self.typeIdFromNative(info.child) },
                .array => |info| return Type { .array = .{ .length = info.len, .element = try self.typeIdFromNative(info.child) } },
                .@"struct" => |info| {
                    var field_types = self.allocator.alloc(TypeId, info.fields.len);
                    errdefer self.allocator.free(field_types);

                    for (info.fields, 0..) |field, i| {
                        field_types[i] = try self.typeIdFromNative(field.type);
                    }

                    return Type { .product = field_types };
                },
                .@"enum" => |info| return self.typeFromNative(info.tag_type),
                .@"union" => |info| {
                    var field_types = self.allocator.alloc(TypeId, info.fields.len);
                    errdefer self.allocator.free(field_types);

                    for (info.fields, 0..) |field, i| {
                        field_types[i] = try self.typeIdFromNative(field.type);
                    }

                    if (info.tag_type) |TT| {
                        return Type { .sum = .{ .discriminator = try self.typeIdFromNative(TT), .types = field_types } };
                    } else {
                        return Type { .raw_sum = field_types };
                    }
                },
                .@"fn" => |info| {
                    const return_type = try self.typeIdFromNative(info.return_type.?);
                    const termination_type = try self.typeIdFromNative(void);

                    const effects = self.allocator.alloc(EvidenceId, 0);
                    errdefer self.allocator.free(effects);

                    var parameter_types = self.allocator.alloc(TypeId, info.param_info.len);
                    errdefer self.allocator.free(parameter_types);

                    for (info.param_info, 0..) |param, i| {
                        parameter_types[i] = try self.typeIdFromNative(param.type);
                    }

                    return Type { .function = .{ .return_type = return_type, .termination_type = termination_type, .effects = effects, .parameter_types = parameter_types } };
                },

                else => @compileError("cannot convert type `" ++ @typeName(T) ++ "` to Type"),
            }
        }
    }

    pub fn typeIdFromNative(self: *IrBuilder, comptime T: type) !TypeId {
        const ty = try self.typeFromNative(T);
        errdefer Type.deinit(ty, self.allocator);

        return self.typeIdPreallocated(ty);
    }

    pub fn getType(self: *IrBuilder, id: TypeId) !Type {
        if (id >= self.type_map.count()) {
            return error.InvalidType;
        }

        return self.type_map.keys()[id];
    }

    /// Calls `allocator.dupe` on the input bytes
    pub fn globalFromBytes(self: *IrBuilder, tyId: TypeId, bytes: []const u8) !GlobalId {
        const dupeBytes = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(dupeBytes);

        return self.globalFromBytesPreallocated(tyId, dupeBytes);
    }

    /// Does not call `allocator.dupe` on the input bytes
    pub fn globalFromBytesPreallocated(self: *IrBuilder, tyId: TypeId, bytes: []u8) !GlobalId {
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

    pub fn globalFromNative(self: *IrBuilder, value: anytype) !GlobalId {
        const T = @TypeOf(value);
        const tyId = try self.typeIdFromNative(T);

        return self.globalFromBytes(tyId, @as([*]const u8, @ptrCast(&value))[0..@sizeOf(T)]);
    }

    pub fn getGlobal(self: *IrBuilder, id: GlobalId) !Global {
        if (id >= self.global_list.items.len) {
            return error.InvalidGlobal;
        }

        return self.global_list.items[id];
    }

    pub fn function(self: *IrBuilder, tyId: TypeId) !*FunctionBuilder {
        const index = self.function_list.items.len;

        if (index >= MAX_FUNCTIONS) {
            return error.TooManyFunctions;
        }

        const builder = try FunctionBuilder.init(self, @truncate(index), tyId);

        try self.function_list.append(self.allocator, builder);

        return builder;
    }

    pub fn getFunction(self: *IrBuilder, id: FunctionId) !*FunctionBuilder {
        if (id >= self.function_list.items.len) {
            return error.InvalidFunction;
        }

        return self.function_list.items[id];
    }

    /// Calls `allocator.dupe` on the input locals
    pub fn foreignFunction(self: *IrBuilder, tyId: TypeId, locals: []TypeId) !ForeignId {
        const dupeLocals = try self.allocator.dupe(TypeId, locals);
        errdefer self.allocator.free(dupeLocals);

        return self.foreignFunctionPreallocated(tyId, dupeLocals);
    }

    /// Does not call `allocator.dupe` on the input locals
    pub fn foreignFunctionPreallocated(self: *IrBuilder, tyId: TypeId, locals: []TypeId) !ForeignId {
        const index = self.foreign_function_list.items.len;

        if (index >= MAX_FUNCTIONS) {
            return error.TooManyForeignFunctions;
        }

        const foreign = ForeignFunction {
            .id = @truncate(index),
            .type = tyId,
            .locals = locals,
        };

        try self.foreign_function_list.append(self.allocator, foreign);

        return foreign.id;
    }

    pub fn getForeignFunction(self: *IrBuilder, id: ForeignId) !ForeignFunction {
        if (id >= self.foreign_function_list.items.len) {
            return error.InvalidForeignFunction;
        }

        return self.foreign_function_list.items[id];
    }

    pub fn evidence(self: *IrBuilder, tyId: TypeId) !EvidenceId {
        const index = self.evidence_list.items.len;

        if (index >= MAX_EVIDENCE) {
            return error.TooManyEvidence;
        }

        try self.evidence_list.append(self.allocator, tyId);

        return @truncate(index);
    }

    pub fn getEvidence(self: *IrBuilder, id: EvidenceId) !TypeId {
        if (id >= self.evidence_list.items.len) {
            return error.InvalidEvidence;
        }

        return self.evidence_list.items[id];
    }
};


test {
    std.testing.refAllDeclsRecursive(@This());
}
