`default_nettype none
`timescale 1ns/1ps
`include "RegFile_defs.svh"

module RegFile#(
    parameter int DATA_WIDTH = 32,
    parameter int REG_NUM = 32
)(
  input  wire logic                         clk,
  input  wire logic [$clog2(REG_NUM) - 1:0] rs1_addr,
  output      logic [DATA_WIDTH - 1:0]      rs1_data,
  input  wire logic [$clog2(REG_NUM) - 1:0] rs2_addr,
  output      logic [DATA_WIDTH - 1:0]      rs2_data,
  input  wire logic [$clog2(REG_NUM) - 1:0] rd_addr,
  input  wire logic [DATA_WIDTH - 1:0]      rd_data,
  input  wire logic                         rd_wren
);
  `include "dfhdl_defs.svh"
  logic [DATA_WIDTH - 1:0] regs [0:REG_NUM - 1];
  always_ff @(posedge clk)
  begin
    rs1_data <= regs[rs1_addr];
  end
  always_ff @(posedge clk)
  begin
    rs2_data <= regs[rs2_addr];
  end
  always_ff @(posedge clk)
  begin
    if (rd_wren) regs[rd_addr] <= rd_data;
    regs[0]  <= {DATA_WIDTH{1'b0}};
  end
endmodule
