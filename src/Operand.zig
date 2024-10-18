const IR = @import("root.zig");


pub const Operand = union(enum) {
    intermediate: Intermediate,
    immediate: Immediate,
    block: IR.BlockId,
    function: IR.FunctionId,
    global: IR.GlobalId,
    upvalue: IR.UpvalueId,
    handler_set: IR.HandlerSetId,
    evidence: IR.EvidenceId,
    local: IR.LocalId,

    pub const Intermediate = packed struct {
        type: IR.TypeId,
        register: IR.RegisterId,
    };

    pub const Immediate = packed struct {
        type: IR.TypeId,
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
