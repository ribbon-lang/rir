const std = @import("std");
const MiscUtils = @import("ZigUtils").Misc;

const IR = @import("root.zig");

const HandlerSet = @This();


parent: *IR.Function,
id: IR.HandlerSetId,
handlers: std.ArrayHashMapUnmanaged(IR.EvidenceId, *IR.Function, MiscUtils.SimpleHashContext, false) = .{},


pub fn init(parent: *IR.Function, id: IR.HandlerSetId) !*HandlerSet {
    const ptr = try parent.root.allocator.create(HandlerSet);

    ptr.* = HandlerSet {
        .parent = parent,
        .id = id,
    };

    return ptr;
}

pub fn handler(self: *HandlerSet, evId: IR.EvidenceId) !*IR.Function {
    if (self.handlers.contains(evId)) {
        return error.EvidenceOverlap;
    }

    const evTy = try self.parent.root.getEvidence(evId);

    const builder = try self.parent.root.function(evTy);

    builder.parent = self.parent;

    try self.handlers.put(self.parent.root.allocator, evId, builder);

    return builder;
}

pub fn getHandler(self: *HandlerSet, evId: IR.EvidenceId) !*IR.Function {
    return self.handlers.get(evId) orelse error.InvalidEvidence;
}
