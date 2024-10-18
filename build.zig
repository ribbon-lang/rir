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

    const Core = b.addModule("Core", .{
        .root_source_file = b.path("src/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Core.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    Core.addImport("ISA", ISA.module("ISA"));
    Core.addImport("Rbc:Core", Rbc.module("Core"));
    Core.addImport("Rbc:Builder", Rbc.module("Builder"));

    const checkStep = b.step("check", "Run semantic analysis");
    const testStep = b.step("test", "Run unit tests");

    const testCore = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
    });

    testCore.root_module.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    testCore.root_module.addImport("ISA", ISA.module("ISA"));
    testCore.root_module.addImport("Rbc:Core", Rbc.module("Core"));
    testCore.root_module.addImport("Rbc:Builder", Rbc.module("Builder"));

    checkStep.dependOn(&testCore.step);

    testStep.dependOn(&b.addRunArtifact(testCore).step);
}
