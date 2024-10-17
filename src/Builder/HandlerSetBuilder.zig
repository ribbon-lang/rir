const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;

const Core = @import("Core");

const Builder = @import("root.zig");
const FunctionBuilder = Builder.FunctionBuilder;


const HandlerSetBuilder = @This();


parent: *FunctionBuilder,
id: Core.HandlerSetId,
handlers: std.ArrayHashMapUnmanaged(Core.EvidenceId, *FunctionBuilder, MiscUtils.SimpleHashContext, false) = .{},


pub fn init(parent: *FunctionBuilder, id: Core.HandlerSetId) !*HandlerSetBuilder {
    const ptr = try parent.root.allocator.create(HandlerSetBuilder);

    ptr.* = HandlerSetBuilder {
        .parent = parent,
        .id = id,
    };

    return ptr;
}

pub fn handler(self: *HandlerSetBuilder, evId: Core.EvidenceId) !*FunctionBuilder {
    if (self.handlers.contains(evId)) {
        return error.EvidenceOverlap;
    }

    const evTy = try self.parent.root.getEvidence(evId);

    const builder = try self.parent.root.function(evTy);

    builder.parent = self.parent;

    try self.handlers.put(self.parent.root.allocator, evId, builder);

    return builder;
}

pub fn getHandler(self: *HandlerSetBuilder, evId: Core.EvidenceId) !*FunctionBuilder {
    return self.handlers.get(evId) orelse error.InvalidEvidence;
}
