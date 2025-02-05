`default_nettype none
`timescale 1ns/1ps
`include "ALU_defs.svh"

module ALU(
  input  wire logic [31:0]  op1,
  input  wire logic [31:0]  op2,
  input  wire t_enum_ALUSel aluSel,
  output      logic [31:0]  aluOut
);
  `include "dfhdl_defs.svh"
  logic [4:0]  shamt;
  logic [31:0] outCalc;
  always_comb
  begin
    case (aluSel)
      ALUSel_ADD:   outCalc = op1 + op2;
      ALUSel_SUB:   outCalc = op1 - op2;
      ALUSel_AND:   outCalc = op1 & op2;
      ALUSel_OR:    outCalc = op1 | op2;
      ALUSel_XOR:   outCalc = op1 ^ op2;
      ALUSel_SLT:   outCalc = {{(32-1){1'b0}}, {$signed(op1) < $signed(op2)}};
      ALUSel_SLTU:  outCalc = {{(32-1){1'b0}}, {op1 < op2}};
      ALUSel_SLL:   outCalc = op1 << shamt;
      ALUSel_SRL:   outCalc = op1 >> shamt;
      ALUSel_SRA:   outCalc = {$signed(op1) >>> shamt};
      ALUSel_COPY1: outCalc = op1;
      default:      outCalc = 32'h????????;
    endcase
  end
  assign shamt  = op2[4:0];
  assign aluOut = outCalc;
endmodule
