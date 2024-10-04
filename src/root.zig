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
pub const VariableWidth = void;

pub const MAX_TYPES = std.math.maxInt(TypeId);
pub const MAX_GLOBALS = std.math.maxInt(GlobalId);
pub const MAX_FUNCTIONS = std.math.maxInt(FunctionId);
pub const MAX_HANDLER_SETS = std.math.maxInt(HandlerSetId);
pub const MAX_EVIDENCE = RbcCore.MAX_EVIDENCE;
pub const MAX_BLOCKS = RbcCore.MAX_BLOCKS;
pub const MAX_REGISTERS = RbcCore.MAX_REGISTERS;

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

pub const BlockBuilder = struct {
    function: *FunctionBuilder,
    parent: ?*BlockBuilder,
    id: BlockId,
    type: TypeId,

    pub fn init(function: *FunctionBuilder, parent: ?*BlockBuilder, id: BlockId, tyId: TypeId) !*BlockBuilder {
        const ptr = try function.root.allocator.create(BlockBuilder);

        ptr.* = BlockBuilder {
            .function = function,
            .parent = parent,
            .id = id,
            .type = tyId,
        };

        return ptr;
    }
};

pub const FunctionBuilder = struct {
    root: *IrBuilder,
    id: FunctionId,
    type: TypeId,
    blocks: std.ArrayListUnmanaged(*BlockBuilder) = .{},

    pub fn init(root: *IrBuilder, id: FunctionId, tyId: TypeId) !*FunctionBuilder {
        const ptr = try root.allocator.create(FunctionBuilder);

        ptr.* = FunctionBuilder {
            .root = root,
            .id = id,
            .type = tyId,
        };

        return ptr;
    }
};

pub const ForeignFunction = struct {
    id: ForeignId,
    type: TypeId,
    locals: []TypeId,
};

pub const Global = struct {
    id: GlobalId,
    type: TypeId,
    value: []u8,
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

pub const HandlerSetBuilder = struct {
    root: *IrBuilder,
    id: HandlerSetId,
    handlers: HandlerMap = .{},

    pub fn init(root: *IrBuilder, id: HandlerSetId) !*HandlerSetBuilder {
        const ptr = try root.allocator.create(HandlerSetBuilder);

        ptr.* = HandlerSetBuilder {
            .root = root,
            .id = id,
        };

        return ptr;
    }

    pub fn handler(self: *HandlerSetBuilder, evId: EvidenceId) !*FunctionBuilder {
        if (self.handlers.contains(evId)) {
            return error.EvidenceOverlap;
        }

        const ev = try self.root.getEvidence(evId);

        const builder = try self.root.function(ev);

        try self.handlers.put(self.root.allocator, evId, builder);

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
    handler_set_list: HandlerSetList = .{},
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

    pub fn handlerSet(self: *IrBuilder) !*HandlerSetBuilder {
        const index = self.handler_set_list.items.len;

        if (index >= MAX_HANDLER_SETS) {
            return error.TooManyHandlerSets;
        }

        const builder = try HandlerSetBuilder.init(self, @truncate(index));

        try self.handler_set_list.append(self.allocator, builder);

        return builder;
    }

    pub fn getHandlerSet(self: *IrBuilder, id: HandlerSetId) !*HandlerSetBuilder {
        if (id >= self.handler_set_list.items.len) {
            return error.InvalidHandlerSet;
        }

        return self.handler_set_list.items[id];
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
