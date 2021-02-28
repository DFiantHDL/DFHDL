`default_nettype	           none
`timescale 1ns/1ps
`include "ParityCheck_defs.v"


module ParityCheck(
  input  wire               clk,
  input  wire               rst,
  input  wire               seqIn,
  output reg                detOut
);
  `define E_fsm_states_Even 0
  `define E_fsm_states_Odd  1
  reg         [0:0]         fsm_state;
  reg         [0:0]         fsm_state_prev1 = `E_fsm_states_Even;
  reg         [0:0]         fsm_state_sig;
  always @(*)
  begin
    fsm_state               = fsm_state_prev1;
    case (fsm_state)
      `E_fsm_states_Even : begin
        detOut              = 1'b1;
        if (seqIn) fsm_state = `E_fsm_states_Odd;
      end
      `E_fsm_states_Odd : begin
        detOut              = 1'b0;
        if (seqIn) fsm_state = `E_fsm_states_Even;
      end
    endcase
    fsm_state_sig           = fsm_state;
  end
  always @(negedge rst or posedge clk)
  begin
    if (rst == 1'b0) fsm_state_prev1 <= `E_fsm_states_Even;
    else fsm_state_prev1 <= fsm_state_sig;
  end
endmodule