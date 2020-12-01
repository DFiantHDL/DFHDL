`default_nettype	            none
`timescale 1ns/1ps
`include "SeqDet_defs.v"


module SeqDet(
  input  wire                clk,
  input  wire                rst,
  input  wire                seqIn,
  output reg                 detOut
);
  reg         [2:0]          fsm_state = `E_fsm_states_S0;
  reg         [2:0]          fsm_state_prev1 = `E_fsm_states_S0;
  reg         [2:0]          fsm_state_sig;
  `define E_fsm_states_S0    0
  `define E_fsm_states_S1    1
  `define E_fsm_states_S10   2
  `define E_fsm_states_S100  3
  `define E_fsm_states_S1001 4
  always @(*)
  begin
    fsm_state                = fsm_state_prev1;
    case (fsm_state)
      `E_fsm_states_S0 : begin
        detOut               = 1'b0;
        if (seqIn) fsm_state = `E_fsm_states_S1;
        else fsm_state = `E_fsm_states_S0;
      end
      `E_fsm_states_S1 : begin
        detOut               = 1'b0;
        if (seqIn) fsm_state = `E_fsm_states_S1;
        else fsm_state = `E_fsm_states_S10;
      end
      `E_fsm_states_S10 : begin
        detOut               = 1'b0;
        if (seqIn) fsm_state = `E_fsm_states_S1;
        else fsm_state = `E_fsm_states_S100;
      end
      `E_fsm_states_S100 : begin
        detOut               = 1'b0;
        if (seqIn) fsm_state = `E_fsm_states_S1001;
        else fsm_state = `E_fsm_states_S0;
      end
      `E_fsm_states_S1001 : begin
        detOut               = 1'b1;
        if (seqIn) fsm_state = `E_fsm_states_S1;
        else fsm_state = `E_fsm_states_S10;
      end
      default : begin
        fsm_state            = 3'b???;
        detOut               = 1'b?;
      end
    endcase
    fsm_state_sig            = fsm_state;
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) fsm_state_prev1 <= `E_fsm_states_S0;
    else fsm_state_prev1 <= fsm_state_sig;
  end
endmodule