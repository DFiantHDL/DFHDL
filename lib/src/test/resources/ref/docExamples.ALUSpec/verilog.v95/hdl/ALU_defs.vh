`ifndef ALU_DEFS
`define ALU_DEFS
`define ALUSel_ADD 0
`define ALUSel_SUB 1
`define ALUSel_SLL 2
`define ALUSel_SRL 3
`define ALUSel_SRA 4
`define ALUSel_AND 5
`define ALUSel_OR 6
`define ALUSel_XOR 7
`define ALUSel_SLT 8
`define ALUSel_SLTU 9
`define ALUSel_COPY1 10


`endif
`ifndef ALU_DEFS_MODULE
`define ALU_DEFS_MODULE
`else
function [8*12:1] ALUSel_to_string;
  input [3:0] value;
  case (value)
    `ALUSel_ADD: ALUSel_to_string = "ALUSel_ADD";
    `ALUSel_SUB: ALUSel_to_string = "ALUSel_SUB";
    `ALUSel_SLL: ALUSel_to_string = "ALUSel_SLL";
    `ALUSel_SRL: ALUSel_to_string = "ALUSel_SRL";
    `ALUSel_SRA: ALUSel_to_string = "ALUSel_SRA";
    `ALUSel_AND: ALUSel_to_string = "ALUSel_AND";
    `ALUSel_OR: ALUSel_to_string = "ALUSel_OR";
    `ALUSel_XOR: ALUSel_to_string = "ALUSel_XOR";
    `ALUSel_SLT: ALUSel_to_string = "ALUSel_SLT";
    `ALUSel_SLTU: ALUSel_to_string = "ALUSel_SLTU";
    `ALUSel_COPY1: ALUSel_to_string = "ALUSel_COPY1";
    default: ALUSel_to_string = "?";
  endcase
endfunction
`undef ALU_DEFS_MODULE
`endif

