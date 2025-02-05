`default_nettype none
`timescale 1ns/1ps
`include "Counter_defs.svh"

module Counter#(parameter int width = 8)(
  input  wire logic clk,
  input  wire logic rst,
  input  wire logic en,
  output      logic [width - 1:0] cnt
);
  `include "dfhdl_defs.svh"
  always_ff @(posedge clk)
  begin
    if (rst == 1'b1) cnt <= width'(0);
    else begin
      if (en) cnt <= cnt + width'(1);
    end
  end
endmodule
