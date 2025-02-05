`ifndef ALU_DEFS
`define ALU_DEFS
typedef enum logic [3:0] {
  ALUSel_ADD   = 0,
  ALUSel_SUB   = 1,
  ALUSel_SLL   = 2,
  ALUSel_SRL   = 3,
  ALUSel_SRA   = 4,
  ALUSel_AND   = 5,
  ALUSel_OR    = 6,
  ALUSel_XOR   = 7,
  ALUSel_SLT   = 8,
  ALUSel_SLTU  = 9,
  ALUSel_COPY1 = 10
} t_enum_ALUSel;

`endif
