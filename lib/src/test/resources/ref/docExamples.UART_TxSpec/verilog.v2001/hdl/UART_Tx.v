`default_nettype none
`timescale 1ns/1ps
`include "UART_Tx_defs.vh"

module UART_Tx#(
    parameter integer CLK_FREQ_KHz = 50000,
    parameter integer BAUD_RATE_BPS = 115200
)(
  input  wire   clk,
  input  wire   rst,
  input  wire   data_en,
  input  wire  [7:0] data,
  output reg    tx,
  output reg    tx_en,
  output reg    tx_done
);
  `include "dfhdl_defs.vh"
  `include "UART_Tx_defs.vh"
  parameter integer BIT_CLOCKS = (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
  `define Status_Idle 1
  `define Status_StartBit 2
  `define Status_DataBits 4
  `define Status_StopBit 8
  `define Status_Finalize 16
  function [8*8:1] Status_to_string;
    input [4:0] value;
    case (value)
      `Status_Idle: Status_to_string = "Idle";
      `Status_StartBit: Status_to_string = "StartBit";
      `Status_DataBits: Status_to_string = "DataBits";
      `Status_StopBit: Status_to_string = "StopBit";
      `Status_Finalize: Status_to_string = "Finalize";
      default: Status_to_string = "?";
    endcase
  endfunction
  reg [4:0] status;
  reg [clog2(BIT_CLOCKS) - 1:0] bitClkCnt;
  reg [2:0] dataBitCnt;
  reg [7:0] shiftData;
  always @(posedge clk)
  begin
    if (rst == 1'b1) begin
      status             <= `Status_Idle;
      bitClkCnt          <= `TO_UNSIGNED(0, 1, clog2(BIT_CLOCKS));
      dataBitCnt         <= 3'd0;
    end
    else begin
      case (status)
        `Status_Idle: begin
          tx_en          <= 1'b0;
          tx             <= 1'b1;
          tx_done        <= 1'b0;
          bitClkCnt      <= `TO_UNSIGNED(0, 1, clog2(BIT_CLOCKS));
          dataBitCnt     <= 3'd0;
          if (data_en) begin
            shiftData    <= data;
            status       <= `Status_StartBit;
          end
        end
        `Status_StartBit: begin
          tx_en          <= 1'b1;
          tx             <= 1'b0;
          if (bitClkCnt == (BIT_CLOCKS - 1)) begin
            bitClkCnt    <= `TO_UNSIGNED(0, 1, clog2(BIT_CLOCKS));
            status       <= `Status_DataBits;
          end
          else bitClkCnt <= bitClkCnt + `TO_UNSIGNED(1, 1, clog2(BIT_CLOCKS));
        end
        `Status_DataBits: begin
          tx             <= shiftData[0];
          if (bitClkCnt == (BIT_CLOCKS - 1)) begin
            bitClkCnt    <= `TO_UNSIGNED(0, 1, clog2(BIT_CLOCKS));
            shiftData    <= shiftData >> 1;
            if (dataBitCnt == 3'd7) begin
              dataBitCnt <= 3'd0;
              status     <= `Status_StopBit;
            end
            else dataBitCnt <= dataBitCnt + 3'd1;
          end
          else bitClkCnt <= bitClkCnt + `TO_UNSIGNED(1, 1, clog2(BIT_CLOCKS));
        end
        `Status_StopBit: begin
          tx             <= 1'b1;
          if (bitClkCnt == (BIT_CLOCKS - 1)) begin
            bitClkCnt    <= `TO_UNSIGNED(0, 1, clog2(BIT_CLOCKS));
            tx_done      <= 1'b1;
            status       <= `Status_Finalize;
          end
          else bitClkCnt <= bitClkCnt + `TO_UNSIGNED(1, 1, clog2(BIT_CLOCKS));
        end
        `Status_Finalize: begin
          tx_en          <= 1'b0;
          tx_done        <= 1'b1;
          status         <= `Status_Idle;
        end
      endcase
    end
  end
endmodule
