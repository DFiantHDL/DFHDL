`ifndef LRSHIFTDIRECT_DEFS
`define LRSHIFTDIRECT_DEFS
`endif
`ifndef LRSHIFTDIRECT_DEFS_MODULE
`define LRSHIFTDIRECT_DEFS_MODULE
`else
`define ShiftDir_Left 0
`define ShiftDir_Right 1

function [8*14:1] ShiftDir_to_string;
  input [0:0] value;
  case (value)
    `ShiftDir_Left: ShiftDir_to_string = "ShiftDir_Left";
    `ShiftDir_Right: ShiftDir_to_string = "ShiftDir_Right";
    default: ShiftDir_to_string = "?";
  endcase
endfunction
`undef LRSHIFTDIRECT_DEFS_MODULE
`endif
