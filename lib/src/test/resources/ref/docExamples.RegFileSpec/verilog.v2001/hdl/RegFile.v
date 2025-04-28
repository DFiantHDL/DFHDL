`default_nettype none
`timescale 1ns/1ps
`include "RegFile_defs.vh"

module RegFile#(
    parameter integer DATA_WIDTH = 32,
    parameter integer REG_NUM = 32
)(
  input  wire                         clk,
  input  wire  [clog2(REG_NUM) - 1:0] rs1_addr,
  output reg [DATA_WIDTH - 1:0]       rs1_data,
  input  wire  [clog2(REG_NUM) - 1:0] rs2_addr,
  output reg [DATA_WIDTH - 1:0]       rs2_data,
  input  wire  [clog2(REG_NUM) - 1:0] rd_addr,
  input  wire  [DATA_WIDTH - 1:0]     rd_data,
  input  wire                         rd_wren
);
  `include "dfhdl_defs.vh"
  `include "RegFile_defs.vh"
  reg [DATA_WIDTH - 1:0] regs [0:REG_NUM - 1];
  always @(posedge clk)
  begin
    rs1_data <= regs[rs1_addr];
  end
  always @(posedge clk)
  begin
    rs2_data <= regs[rs2_addr];
  end
  always @(posedge clk)
  begin
    if (rd_wren) regs[rd_addr] <= rd_data;
    regs[0]  <= {DATA_WIDTH{1'b0}};
  end
endmodule
