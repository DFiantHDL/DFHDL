`ifndef LRSHIFTFLAT_DEFS
`define LRSHIFTFLAT_DEFS
`define ShiftDir_Left 0
`define ShiftDir_Right 1


`endif
`ifndef LRSHIFTFLAT_DEFS_MODULE
`define LRSHIFTFLAT_DEFS_MODULE
`else
function [8*5:1] ShiftDir_to_string;
  input [0:0] value;
  case (value)
    `ShiftDir_Left: ShiftDir_to_string = "Left";
    `ShiftDir_Right: ShiftDir_to_string = "Right";
    default: ShiftDir_to_string = "?";
  endcase
endfunction
`undef LRSHIFTFLAT_DEFS_MODULE
`endif

