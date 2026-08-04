`default_nettype none
`timescale 1ns/1ps

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
  /* verilator lint_off MULTIDRIVEN */
  logic [DATA_WIDTH - 1:0] ram [0:(2 ** ADDR_WIDTH) - 1];
  /* verilator lint_on MULTIDRIVEN */
  always_ff @(posedge a_clk)
  begin
    a_q <= ram[a_addr];
    if (a_we) ram[a_addr] <= a_data;
  end
  always_ff @(posedge b_clk)
  begin
    b_q <= ram[b_addr];
    if (b_we) ram[b_addr] <= b_data;
  end
endmodule
