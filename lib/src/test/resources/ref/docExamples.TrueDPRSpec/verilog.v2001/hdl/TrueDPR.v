`default_nettype none
`timescale 1ns/1ps

module TrueDPR#(
    parameter integer DATA_WIDTH = 8,
    parameter integer ADDR_WIDTH = 8
)(
  input  wire                    a_clk,
  input  wire [DATA_WIDTH - 1:0] a_data,
  input  wire [ADDR_WIDTH - 1:0] a_addr,
  output reg [DATA_WIDTH - 1:0]  a_q,
  input  wire                    a_we,
  input  wire                    b_clk,
  input  wire [DATA_WIDTH - 1:0] b_data,
  input  wire [ADDR_WIDTH - 1:0] b_addr,
  output reg [DATA_WIDTH - 1:0]  b_q,
  input  wire                    b_we
);
  `include "dfhdl_defs.vh"
  /* verilator lint_off MULTIDRIVEN */
  reg [DATA_WIDTH - 1:0] ram [0:(2 ** ADDR_WIDTH) - 1];
  /* verilator lint_on MULTIDRIVEN */
  always @(posedge a_clk)
  begin
    if (a_we) ram[a_addr] <= a_data;
    a_q <= ram[a_addr];
  end
  always @(posedge b_clk)
  begin
    if (b_we) ram[b_addr] <= b_data;
    b_q <= ram[b_addr];
  end
endmodule
