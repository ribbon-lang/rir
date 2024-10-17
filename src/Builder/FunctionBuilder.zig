const std = @import("std");

const Core = @import("Core");

const Builder = @import("root.zig");
const BlockBuilder = Builder.BlockBuilder;
const HandlerSetBuilder = Builder.HandlerSetBuilder;


const FunctionBuilder = @This();


root: *Builder,
id: Core.FunctionId,
type: Core.TypeId,
blocks: std.ArrayListUnmanaged(*BlockBuilder),
local_types: std.ArrayListUnmanaged(Core.TypeId),
parent: ?*FunctionBuilder = null,
upvalue_indices: std.ArrayListUnmanaged(Core.LocalId) = .{},
handler_sets: std.ArrayListUnmanaged(*HandlerSetBuilder) = .{},


pub fn init(root: *Builder, id: Core.FunctionId, tyId: Core.TypeId) !*FunctionBuilder {
    const ptr = try root.allocator.create(FunctionBuilder);

    const ty = try root.getType(tyId);
    const funcTy = try ty.asFunction();

    var blocks = std.ArrayListUnmanaged(*BlockBuilder){};
    errdefer blocks.deinit(root.allocator);

    const entryBlock = try BlockBuilder.init(ptr, null, 0);

    try blocks.append(root.allocator, entryBlock);

    var local_types = std.ArrayListUnmanaged(Core.TypeId){};
    errdefer local_types.deinit(root.allocator);

    for (funcTy.parameter_types) |param| {
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

pub fn local(self: *FunctionBuilder, tyId: Core.TypeId) !Core.LocalId {
    const index = self.local_types.items.len;

    if (index >= Core.MAX_LOCALS) {
        return error.TooManyLocals;
    }

    try self.local_types.append(self.root.allocator, tyId);

    return @truncate(index);
}

pub fn getLocalType(self: *const FunctionBuilder, l: Core.LocalId) !Core.TypeId {
    if (l >= self.local_types.items.len) {
        return error.InvalidRegister;
    }

    return self.local_types.items[l];
}

pub fn upvalue(self: *FunctionBuilder, parentLocal: Core.LocalId) !Core.UpvalueId {
    if (self.parent) |parent| {
        _ = try parent.getLocalType(parentLocal);

        const index = self.upvalue_indices.items.len;

        if (index >= Core.MAX_LOCALS) {
            return error.TooManyUpvalues;
        }

        try self.upvalue_indices.append(self.root.allocator, parentLocal);

        return @truncate(index);
    } else {
        return error.InvalidUpvalue;
    }
}

pub fn getUpvalueType(self: *const FunctionBuilder, u: Core.UpvalueId) !Core.TypeId {
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

    if (index >= Core.MAX_BLOCKS) {
        return error.TooManyBlocks;
    }

    const newBlock = try BlockBuilder.init(self, parent, @truncate(index));

    try self.blocks.append(self.root.allocator, newBlock);

    return newBlock;
}

pub fn getBlock(self: *FunctionBuilder, id: Core.BlockId) !*BlockBuilder {
    if (id >= self.blocks.items.len) {
        return error.InvalidBlock;
    }

    return self.blocks.items[id];
}

pub fn handlerSet(self: *FunctionBuilder) !*HandlerSetBuilder {
    const index = self.handler_sets.items.len;

    if (index >= Core.MAX_HANDLER_SETS) {
        return error.TooManyHandlerSets;
    }

    const builder = try HandlerSetBuilder.init(self, @truncate(index));

    try self.handler_sets.append(self.root.allocator, builder);

    return builder;
}

pub fn getHandlerSet(self: *FunctionBuilder, id: Core.HandlerSetId) !*HandlerSetBuilder {
    if (id >= self.handler_sets.items.len) {
        return error.InvalidHandlerSet;
    }

    return self.handler_sets.items[id];
}
