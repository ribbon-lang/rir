const Core = @import("root.zig");

pub const Operand = union(enum) {
    intermediate: Intermediate,
    immediate: Immediate,
    block: Core.BlockId,
    function: Core.FunctionId,
    global: Core.GlobalId,
    upvalue: Core.UpvalueId,
    handler_set: Core.HandlerSetId,
    evidence: Core.EvidenceId,
    local: Core.LocalId,

    pub const Intermediate = packed struct {
        type: Core.TypeId,
        register: Core.RegisterId,
    };

    pub const Immediate = packed struct {
        type: Core.TypeId,
        value: u64,
    };

    pub fn isConstant(self: Operand) bool {
        return switch (self) {
            inline
                .intermediate,
                .global,
                .upvalue,
                .local,
            => false,

            inline
                .immediate,
                .block,
                .function,
                .handler_set,
                .evidence,
             => true,
        };
    }
};
