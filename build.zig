const std = @import("std");

pub fn build(b: *std.Build) !void {
    const defaultTarget = b.standardTargetOptions(.{});
    const defaultOptimize = b.standardOptimizeOption(.{});

    const ZigUtils = b.dependency("ZigUtils", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const ISA = b.dependency("ISA", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const Rbc = b.dependency("Rbc", .{
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    const Rir = b.addModule("Rir", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Rir.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    Rir.addImport("ISA", ISA.module("ISA"));
    Rir.addImport("Rbc:Core", Rbc.module("Core"));
    Rir.addImport("Rbc:Builder", Rbc.module("Builder"));


    const testStep = b.step("test", "Run unit tests");

    const testExe = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
    });

    testExe.root_module.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    testExe.root_module.addImport("ISA", ISA.module("ISA"));
    testExe.root_module.addImport("Rbc:Core", Rbc.module("Core"));
    testExe.root_module.addImport("Rbc:Builder", Rbc.module("Builder"));

    testStep.dependOn(&b.addRunArtifact(testExe).step);
}
