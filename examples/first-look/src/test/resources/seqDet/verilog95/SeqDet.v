`default_nettype	               none
`timescale 1ns/1ps
`include "SeqDet_defs.v"


module SeqDet(
  input  wire                   clk,
  input  wire                   rst,
  input  wire                   seqIn,
  output reg                    detOut
);
  reg         [2:0]             detFsm_state = `E_detFsm_states_S0;
  reg         [2:0]             detFsm_state_prev1 = `E_detFsm_states_S0;
  reg         [2:0]             detFsm_state_sig;
  `define E_detFsm_states_S0    0
  `define E_detFsm_states_S1    1
  `define E_detFsm_states_S10   2
  `define E_detFsm_states_S100  3
  `define E_detFsm_states_S1001 4
  always @(clk or detFsm_state_prev1 or seqIn or rst)
  begin
    detFsm_state                = detFsm_state_prev1;
    case (detFsm_state)
      `E_detFsm_states_S0 : begin
        detOut                  = 1'b0;
        if (seqIn) detFsm_state = `E_detFsm_states_S1;
        else detFsm_state = `E_detFsm_states_S0;
      end
      `E_detFsm_states_S1 : begin
        detOut                  = 1'b0;
        if (seqIn) detFsm_state = `E_detFsm_states_S1;
        else detFsm_state = `E_detFsm_states_S10;
      end
      `E_detFsm_states_S10 : begin
        detOut                  = 1'b0;
        if (seqIn) detFsm_state = `E_detFsm_states_S1;
        else detFsm_state = `E_detFsm_states_S100;
      end
      `E_detFsm_states_S100 : begin
        detOut                  = 1'b0;
        if (seqIn) detFsm_state = `E_detFsm_states_S1001;
        else detFsm_state = `E_detFsm_states_S0;
      end
      `E_detFsm_states_S1001 : begin
        detOut                  = 1'b1;
        if (seqIn) detFsm_state = `E_detFsm_states_S1;
        else detFsm_state = `E_detFsm_states_S10;
      end
      default : begin
        detFsm_state            = 3'b???;
        detOut                  = 1'b?;
      end
    endcase
    detFsm_state_sig            = detFsm_state;
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) detFsm_state_prev1 <= `E_detFsm_states_S0;
    else detFsm_state_prev1 <= detFsm_state_sig;
  end
endmodule