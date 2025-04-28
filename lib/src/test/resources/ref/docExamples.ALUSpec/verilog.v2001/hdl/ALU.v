`default_nettype none
`timescale 1ns/1ps
`include "ALU_defs.vh"

module ALU(
  input  wire  [31:0] op1,
  input  wire  [31:0] op2,
  input  wire  [3:0]  aluSel,
  output reg [31:0]   aluOut
);
  `include "dfhdl_defs.vh"
  `include "ALU_defs.vh"
  wire [4:0] shamt;
  always @(*)
  begin
    case (aluSel)
      `ALUSel_ADD: aluOut = op1 + op2;
      `ALUSel_SUB: aluOut = op1 - op2;
      `ALUSel_AND: aluOut = op1 & op2;
      `ALUSel_OR: aluOut = op1 | op2;
      `ALUSel_XOR: aluOut = op1 ^ op2;
      `ALUSel_SLT: aluOut = {{(32-1){1'b0}}, {$signed(op1) < $signed(op2)}};
      `ALUSel_SLTU: aluOut = {{(32-1){1'b0}}, {op1 < op2}};
      `ALUSel_SLL: aluOut = op1 << shamt;
      `ALUSel_SRL: aluOut = op1 >> shamt;
      `ALUSel_SRA: aluOut = {$signed(op1) >>> shamt};
      `ALUSel_COPY1: aluOut = op1;
      default: aluOut = 32'h????????;
    endcase
  end
  assign shamt = op2[4:0];
endmodule
