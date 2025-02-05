`default_nettype none
`timescale 1ns/1ps
`include "UART_Tx_defs.svh"

module UART_Tx#(
    parameter int CLK_FREQ_KHz = 50000,
    parameter int BAUD_RATE_BPS = 115200
)(
  input  wire logic clk,
  input  wire logic rst,
  input  wire logic data_en,
  input  wire logic [7:0] data,
  output      logic tx,
  output      logic tx_en,
  output      logic tx_done
);
  `include "dfhdl_defs.svh"
  parameter int BIT_CLOCKS = (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
  typedef enum {
    Status_Idle     = 1,
    Status_StartBit = 2,
    Status_DataBits = 4,
    Status_StopBit  = 8,
    Status_Finalize = 16
  } t_enum_Status;
  t_enum_Status status;
  logic [$clog2(BIT_CLOCKS) - 1:0] bitClkCnt;
  logic [2:0]   dataBitCnt;
  logic [7:0]   shiftData;
  always_ff @(posedge clk)
  begin
    if (rst == 1'b1) begin
      status             <= Status_Idle;
      bitClkCnt          <= $clog2(BIT_CLOCKS)'(0);
      dataBitCnt         <= 3'd0;
    end
    else begin
      case (status)
        Status_Idle:     begin
          tx_en          <= 1'b0;
          tx             <= 1'b1;
          tx_done        <= 1'b0;
          bitClkCnt      <= $clog2(BIT_CLOCKS)'(0);
          dataBitCnt     <= 3'd0;
          if (data_en) begin
            shiftData    <= data;
            status       <= Status_StartBit;
          end
        end
        Status_StartBit: begin
          tx_en          <= 1'b1;
          tx             <= 1'b0;
          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
            status       <= Status_DataBits;
          end
          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
        end
        Status_DataBits: begin
          tx             <= shiftData[0];
          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
            shiftData    <= shiftData >> 1;
            if (dataBitCnt == 3'd7) begin
              dataBitCnt <= 3'd0;
              status     <= Status_StopBit;
            end
            else dataBitCnt <= dataBitCnt + 3'd1;
          end
          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
        end
        Status_StopBit:  begin
          tx             <= 1'b1;
          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
            tx_done      <= 1'b1;
            status       <= Status_Finalize;
          end
          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
        end
        Status_Finalize: begin
          tx_en          <= 1'b0;
          tx_done        <= 1'b1;
          status         <= Status_Idle;
        end
      endcase
    end
  end
endmodule
