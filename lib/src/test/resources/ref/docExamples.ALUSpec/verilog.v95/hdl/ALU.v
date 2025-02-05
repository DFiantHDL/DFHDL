`default_nettype none
`timescale 1ns/1ps
`include "ALU_defs.vh"

module ALU(
  op1,
  op2,
  aluSel,
  aluOut
);
  `include "dfhdl_defs.vh"
  input  wire  [31:0] op1;
  input  wire  [31:0] op2;
  input  wire  [3:0]  aluSel;
  output wire [31:0]  aluOut;
  wire [4:0] shamt;
  reg [31:0] outCalc;
  always @(aluSel or op1 or op2 or shamt)
  begin
    case (aluSel)
      `ALUSel_ADD: outCalc = op1 + op2;
      `ALUSel_SUB: outCalc = op1 - op2;
      `ALUSel_AND: outCalc = op1 & op2;
      `ALUSel_OR: outCalc = op1 | op2;
      `ALUSel_XOR: outCalc = op1 ^ op2;
      `ALUSel_SLT: outCalc = {{(32-1){1'b0}}, {`SIGNED_LESS_THAN(op1, op2, 32)}};
      `ALUSel_SLTU: outCalc = {{(32-1){1'b0}}, {op1 < op2}};
      `ALUSel_SLL: outCalc = op1 << shamt;
      `ALUSel_SRL: outCalc = op1 >> shamt;
      `ALUSel_SRA: outCalc = {`SIGNED_SHIFT_RIGHT(op1, shamt, 32)};
      `ALUSel_COPY1: outCalc = op1;
      default: outCalc = 32'h????????;
    endcase
  end
  assign shamt  = op2[4:0];
  assign aluOut = outCalc;
endmodule
