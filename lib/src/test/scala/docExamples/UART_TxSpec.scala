package docExamples

class UART_TxSpec extends util.FullCompileSpec:
  def dut = uart_tx.UART_Tx()

  def expectedVerilogCS =
    """|`default_nettype none
       |`timescale 1ns/1ps
       |`include "UART_Tx_defs.svh"
       |
       |module UART_Tx#(
       |    parameter int CLK_FREQ_KHz = 50000,
       |    parameter int BAUD_RATE_BPS = 115200
       |)(
       |  input  wire logic clk,
       |  input  wire logic rst,
       |  input  wire logic data_en,
       |  input  wire logic [7:0] data,
       |  output      logic tx,
       |  output      logic tx_en,
       |  output      logic tx_done
       |);
       |  typedef enum {
       |    Status_Idle     = 1,
       |    Status_StartBit = 2,
       |    Status_DataBits = 4,
       |    Status_StopBit  = 8,
       |    Status_Finalize = 16
       |  } t_enum_Status;
       |  parameter int BIT_CLOCKS = (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
       |  t_enum_Status status;
       |  logic [$clog2(BIT_CLOCKS) - 1:0] bitClkCnt;
       |  logic [2:0]   dataBitCnt;
       |  logic [7:0]   shiftData;
       |  always_ff @(posedge clk)
       |  begin
       |    if (rst == 1'b1) begin
       |      status             <= Status_Idle;
       |      bitClkCnt          <= $clog2(BIT_CLOCKS)'(0);
       |      dataBitCnt         <= 3'd0;
       |    end
       |    else begin
       |      case (status)
       |        Status_Idle:     begin
       |          tx_en          <= 1'b0;
       |          tx             <= 1'b1;
       |          tx_done        <= 1'b0;
       |          bitClkCnt      <= $clog2(BIT_CLOCKS)'(0);
       |          dataBitCnt     <= 3'd0;
       |          if (data_en) begin
       |            shiftData    <= data;
       |            status       <= Status_StartBit;
       |          end
       |        end
       |        Status_StartBit: begin
       |          tx_en          <= 1'b1;
       |          tx             <= 1'b0;
       |          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
       |            status       <= Status_DataBits;
       |          end
       |          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |        end
       |        Status_DataBits: begin
       |          tx             <= shiftData[0];
       |          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
       |            shiftData    <= shiftData >> 1;
       |            if (dataBitCnt == 3'd7) begin
       |              dataBitCnt <= 3'd0;
       |              status     <= Status_StopBit;
       |            end
       |            else dataBitCnt <= dataBitCnt + 3'd1;
       |          end
       |          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |        end
       |        Status_StopBit:  begin
       |          tx             <= 1'b1;
       |          if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |            bitClkCnt    <= $clog2(BIT_CLOCKS)'(0);
       |            tx_done      <= 1'b1;
       |            status       <= Status_Finalize;
       |          end
       |          else bitClkCnt <= bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |        end
       |        Status_Finalize: begin
       |          tx_en          <= 1'b0;
       |          tx_done        <= 1'b1;
       |          status         <= Status_Idle;
       |        end
       |      endcase
       |    end
       |  end
       |endmodule""".stripMargin

  def expectedVHDLCS =
    """|library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |use work.UART_Tx_pkg.all;
       |
       |entity UART_Tx is
       |generic (
       |  CLK_FREQ_KHz : integer := 50000;
       |  BAUD_RATE_BPS : integer := 115200
       |);
       |port (
       |  clk     : in  std_logic;
       |  rst     : in  std_logic;
       |  data_en : in  std_logic;
       |  data    : in  std_logic_vector(7 downto 0);
       |  tx      : out std_logic;
       |  tx_en   : out std_logic;
       |  tx_done : out std_logic
       |);
       |end UART_Tx;
       |
       |architecture UART_Tx_arch of UART_Tx is
       |  type t_enum_Status is (
       |    Status_Idle, Status_StartBit, Status_DataBits, Status_StopBit, Status_Finalize
       |  );
       |  constant BIT_CLOCKS : integer := (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
       |  signal status       : t_enum_Status;
       |  signal bitClkCnt    : unsigned(clog2(BIT_CLOCKS) - 1 downto 0);
       |  signal dataBitCnt   : unsigned(2 downto 0);
       |  signal shiftData    : std_logic_vector(7 downto 0);
       |begin
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then
       |      if rst = '1' then
       |        status             <= Status_Idle;
       |        bitClkCnt          <= resize(d"0", clog2(BIT_CLOCKS));
       |        dataBitCnt         <= 3d"0";
       |      else
       |        case status is
       |          when Status_Idle     =>
       |            tx_en          <= '0';
       |            tx             <= '1';
       |            tx_done        <= '0';
       |            bitClkCnt      <= resize(d"0", clog2(BIT_CLOCKS));
       |            dataBitCnt     <= 3d"0";
       |            if data_en then
       |              shiftData    <= data;
       |              status       <= Status_StartBit;
       |            end if;
       |          when Status_StartBit =>
       |            tx_en          <= '1';
       |            tx             <= '0';
       |            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
       |              status       <= Status_DataBits;
       |            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |            end if;
       |          when Status_DataBits =>
       |            tx             <= shiftData(0);
       |            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
       |              shiftData    <= slv_srl(shiftData, 1);
       |              if dataBitCnt = 3d"7" then
       |                dataBitCnt <= 3d"0";
       |                status     <= Status_StopBit;
       |              else dataBitCnt <= dataBitCnt + 3d"1";
       |              end if;
       |            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |            end if;
       |          when Status_StopBit  =>
       |            tx             <= '1';
       |            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
       |              tx_done      <= '1';
       |              status       <= Status_Finalize;
       |            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |            end if;
       |          when Status_Finalize =>
       |            tx_en          <= '0';
       |            tx_done        <= '1';
       |            status         <= Status_Idle;
       |        end case;
       |      end if;
       |    end if;
       |  end process;
       |end UART_Tx_arch;""".stripMargin
end UART_TxSpec
