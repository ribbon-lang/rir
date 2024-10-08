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
        .root_source_file = b.path("src/Core/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Core.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    Core.addImport("ISA", ISA.module("ISA"));
    Core.addImport("Rbc:Core", Rbc.module("Core"));
    Core.addImport("Rbc:Builder", Rbc.module("Builder"));

    const Builder = b.addModule("Builder", .{
        .root_source_file = b.path("src/Builder/root.zig"),
        .target = defaultTarget,
        .optimize = defaultOptimize,
    });

    Builder.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    Builder.addImport("Core", Core);

    const checkStep = b.step("check", "Run semantic analysis");
    const testStep = b.step("test", "Run unit tests");

    const testCore = b.addTest(.{
        .root_source_file = b.path("src/Core/root.zig"),
    });

    testCore.root_module.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    testCore.root_module.addImport("ISA", ISA.module("ISA"));
    testCore.root_module.addImport("Rbc:Core", Rbc.module("Core"));
    testCore.root_module.addImport("Rbc:Builder", Rbc.module("Builder"));

    const testBuilder = b.addTest(.{
        .root_source_file = b.path("src/Builder/root.zig"),
    });

    testBuilder.root_module.addImport("ZigUtils", ZigUtils.module("ZigUtils"));
    testBuilder.root_module.addImport("Core", &testCore.root_module);

    checkStep.dependOn(&testCore.step);
    checkStep.dependOn(&testBuilder.step);

    testStep.dependOn(&b.addRunArtifact(testCore).step);
    testStep.dependOn(&b.addRunArtifact(testBuilder).step);
}
