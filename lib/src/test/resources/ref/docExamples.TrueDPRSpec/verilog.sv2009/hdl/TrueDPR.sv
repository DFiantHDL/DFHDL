`default_nettype none
`timescale 1ns/1ps
`include "TrueDPR_defs.svh"

module TrueDPR#(
    parameter int DATA_WIDTH = 8,
    parameter int ADDR_WIDTH = 8
)(
  input  wire logic                    a_clk,
  input  wire logic [DATA_WIDTH - 1:0] a_data,
  input  wire logic [ADDR_WIDTH - 1:0] a_addr,
  output      logic [DATA_WIDTH - 1:0] a_q,
  input  wire logic                    a_we,
  input  wire logic                    b_clk,
  input  wire logic [DATA_WIDTH - 1:0] b_data,
  input  wire logic [ADDR_WIDTH - 1:0] b_addr,
  output      logic [DATA_WIDTH - 1:0] b_q,
  input  wire logic                    b_we
);
  `include "dfhdl_defs.svh"
  logic [DATA_WIDTH - 1:0] ram [0:2 ** ADDR_WIDTH - 1];
  always_ff @(posedge a_clk)
  begin
    if (a_we) begin
      /* verilator lint_off BLKSEQ */
      ram[a_addr] = a_data;
      /* verilator lint_on BLKSEQ */
    end
    a_q           <= ram[a_addr];
  end
  always_ff @(posedge b_clk)
  begin
    if (b_we) begin
      /* verilator lint_off BLKSEQ */
      ram[b_addr] = b_data;
      /* verilator lint_on BLKSEQ */
    end
    b_q           <= ram[b_addr];
  end
endmodule
