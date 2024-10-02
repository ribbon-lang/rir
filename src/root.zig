const std = @import("std");
const ISA = @import("ISA");
const RbcCore = @import("Rbc:Core");
const RbcBuilder = @import("Rbc:Builder");


test {
    std.testing.refAllDeclsRecursive(@This());
}

test {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var builder = try RbcBuilder.init(allocator);

    _ = try builder.assemble(allocator);
}
