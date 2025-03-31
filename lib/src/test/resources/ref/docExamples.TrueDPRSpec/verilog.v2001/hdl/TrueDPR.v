`default_nettype none
`timescale 1ns/1ps
`include "TrueDPR_defs.vh"

module TrueDPR#(
    parameter integer DATA_WIDTH = 8,
    parameter integer ADDR_WIDTH = 8
)(
  input  wire                     a_clk,
  input  wire  [DATA_WIDTH - 1:0] a_data,
  input  wire  [ADDR_WIDTH - 1:0] a_addr,
  output reg [DATA_WIDTH - 1:0]   a_q,
  input  wire                     a_we,
  input  wire                     b_clk,
  input  wire  [DATA_WIDTH - 1:0] b_data,
  input  wire  [ADDR_WIDTH - 1:0] b_addr,
  output reg [DATA_WIDTH - 1:0]   b_q,
  input  wire                     b_we
);
  `include "dfhdl_defs.vh"
  `include "TrueDPR_defs.vh"
  reg [DATA_WIDTH - 1:0] ram [0:2 ** ADDR_WIDTH - 1];
  always @(posedge a_clk)
  begin
    if (a_we) begin
      /* verilator lint_off BLKSEQ */
      ram[a_addr] = a_data;
      /* verilator lint_on BLKSEQ */
    end
    a_q           <= ram[a_addr];
  end
  always @(posedge b_clk)
  begin
    if (b_we) begin
      /* verilator lint_off BLKSEQ */
      ram[b_addr] = b_data;
      /* verilator lint_on BLKSEQ */
    end
    b_q           <= ram[b_addr];
  end
endmodule
