const std = @import("std");

const IR = @import("root.zig");

const Function = @This();


root: *IR,
id: IR.FunctionId,
type: IR.TypeId,
blocks: std.ArrayListUnmanaged(*IR.Block),
local_types: std.ArrayListUnmanaged(IR.TypeId),
parent: ?*Function = null,
upvalue_indices: std.ArrayListUnmanaged(IR.LocalId) = .{},
handler_sets: std.ArrayListUnmanaged(*IR.HandlerSet) = .{},


pub fn init(root: *IR, id: IR.FunctionId, tyId: IR.TypeId) !*Function {
    const ptr = try root.allocator.create(Function);

    const ty = try root.getType(tyId);
    const funcTy = try ty.asFunction();

    var blocks = std.ArrayListUnmanaged(*IR.Block){};
    errdefer blocks.deinit(root.allocator);

    const entryBlock = try IR.Block.init(ptr, null, 0);

    try blocks.append(root.allocator, entryBlock);

    var local_types = std.ArrayListUnmanaged(IR.TypeId){};
    errdefer local_types.deinit(root.allocator);

    for (funcTy.parameter_types) |param| {
        try local_types.append(root.allocator, param);
    }

    ptr.* = Function {
        .root = root,
        .id = id,
        .type = tyId,
        .blocks = blocks,
        .local_types = local_types,
    };

    return ptr;
}

pub fn local(self: *Function, tyId: IR.TypeId) !IR.LocalId {
    const index = self.local_types.items.len;

    if (index >= IR.MAX_LOCALS) {
        return error.TooManyLocals;
    }

    try self.local_types.append(self.root.allocator, tyId);

    return @truncate(index);
}

pub fn getLocalType(self: *const Function, l: IR.LocalId) !IR.TypeId {
    if (l >= self.local_types.items.len) {
        return error.InvalidRegister;
    }

    return self.local_types.items[l];
}

pub fn upvalue(self: *Function, parentLocal: IR.LocalId) !IR.UpvalueId {
    if (self.parent) |parent| {
        _ = try parent.getLocalType(parentLocal);

        const index = self.upvalue_indices.items.len;

        if (index >= IR.MAX_LOCALS) {
            return error.TooManyUpvalues;
        }

        try self.upvalue_indices.append(self.root.allocator, parentLocal);

        return @truncate(index);
    } else {
        return error.InvalidUpvalue;
    }
}

pub fn getUpvalueType(self: *const Function, u: IR.UpvalueId) !IR.TypeId {
    if (self.parent) |parent| {
        if (u >= self.upvalue_indices.items.len) {
            return error.InvalidRegister;
        }

        return parent.getLocalType(self.upvalue_indices.items[u]);
    } else {
        return error.InvalidUpvalue;
    }
}

pub fn entry(self: *Function) !*IR.Block {
    return self.blocks.items[0];
}

pub fn block(self: *Function, parent: *IR.Block) !*IR.Block {
    const index = self.blocks.items.len;

    if (index >= IR.MAX_BLOCKS) {
        return error.TooManyBlocks;
    }

    const newBlock = try IR.Block.init(self, parent, @truncate(index));

    try self.blocks.append(self.root.allocator, newBlock);

    return newBlock;
}

pub fn getBlock(self: *Function, id: IR.BlockId) !*IR.Block {
    if (id >= self.blocks.items.len) {
        return error.InvalidBlock;
    }

    return self.blocks.items[id];
}

pub fn handlerSet(self: *Function) !*IR.HandlerSet {
    const index = self.handler_sets.items.len;

    if (index >= IR.MAX_HANDLER_SETS) {
        return error.TooManyHandlerSets;
    }

    const builder = try IR.HandlerSet.init(self, @truncate(index));

    try self.handler_sets.append(self.root.allocator, builder);

    return builder;
}

pub fn getHandlerSet(self: *Function, id: IR.HandlerSetId) !*IR.HandlerSet {
    if (id >= self.handler_sets.items.len) {
        return error.InvalidHandlerSet;
    }

    return self.handler_sets.items[id];
}
