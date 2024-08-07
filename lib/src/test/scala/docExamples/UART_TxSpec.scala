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
       |  output logic tx,
       |  output logic tx_en,
       |  output logic tx_done
       |);
       |  typedef enum {
       |    Status_Idle            = 1,
       |    Status_StartBit        = 2,
       |    Status_DataBits        = 4,
       |    Status_StopBit         = 8,
       |    Status_Finalize        = 16
       |  } t_enum_Status;
       |  parameter int BIT_CLOCKS = (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
       |  t_enum_Status status;
       |  logic [$clog2(BIT_CLOCKS) - 1:0] bitClkCnt;
       |  logic [2:0]   dataBitCnt;
       |  logic [7:0]   shiftData;
       |  logic         tx_din;
       |  logic         tx_en_din;
       |  logic         tx_done_din;
       |  t_enum_Status status_din;
       |  logic [$clog2(BIT_CLOCKS) - 1:0] bitClkCnt_din;
       |  logic [2:0]   dataBitCnt_din;
       |  logic [7:0]   shiftData_din;
       |  always_comb
       |  begin
       |    tx_din                 = tx;
       |    tx_en_din              = tx_en;
       |    tx_done_din            = tx_done;
       |    status_din             = status;
       |    bitClkCnt_din          = bitClkCnt;
       |    dataBitCnt_din         = dataBitCnt;
       |    shiftData_din          = shiftData;
       |    case (status)
       |      Status_Idle:     begin
       |        tx_en_din          = 1'b0;
       |        tx_din             = 1'b1;
       |        tx_done_din        = 1'b0;
       |        bitClkCnt_din      = $clog2(BIT_CLOCKS)'(0);
       |        dataBitCnt_din     = 3'd0;
       |        if (data_en) begin
       |          shiftData_din    = data;
       |          status_din       = Status_StartBit;
       |        end
       |      end
       |      Status_StartBit: begin
       |        tx_en_din          = 1'b1;
       |        tx_din             = 1'b0;
       |        if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |          bitClkCnt_din    = $clog2(BIT_CLOCKS)'(0);
       |          status_din       = Status_DataBits;
       |        end
       |        else bitClkCnt_din = bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |      end
       |      Status_DataBits: begin
       |        tx_din             = shiftData[0];
       |        if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |          bitClkCnt_din    = $clog2(BIT_CLOCKS)'(0);
       |          shiftData_din    = shiftData >> 1;
       |          if (dataBitCnt == 3'd7) begin
       |            dataBitCnt_din = 3'd0;
       |            status_din     = Status_StopBit;
       |          end
       |          else dataBitCnt_din = dataBitCnt + 3'd1;
       |        end
       |        else bitClkCnt_din = bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |      end
       |      Status_StopBit:  begin
       |        tx_din             = 1'b1;
       |        if (bitClkCnt == $clog2(BIT_CLOCKS)'(BIT_CLOCKS - 1)) begin
       |          bitClkCnt_din    = $clog2(BIT_CLOCKS)'(0);
       |          tx_done_din      = 1'b1;
       |          status_din       = Status_Finalize;
       |        end
       |        else bitClkCnt_din = bitClkCnt + $clog2(BIT_CLOCKS)'(1);
       |      end
       |      Status_Finalize: begin
       |        tx_en_din          = 1'b0;
       |        tx_done_din        = 1'b1;
       |        status_din         = Status_Idle;
       |      end
       |    endcase
       |  end
       |  always_ff @(posedge clk)
       |  begin
       |    if (rst == 1'b1) begin
       |      status               <= Status_Idle;
       |      bitClkCnt            <= $clog2(BIT_CLOCKS)'(0);
       |      dataBitCnt           <= 3'd0;
       |    end
       |    else begin
       |      tx                   <= tx_din;
       |      tx_en                <= tx_en_din;
       |      tx_done              <= tx_done_din;
       |      status               <= status_din;
       |      bitClkCnt            <= bitClkCnt_din;
       |      dataBitCnt           <= dataBitCnt_din;
       |      shiftData            <= shiftData_din;
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
       |  constant BIT_CLOCKS   : integer := (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
       |  signal status         : t_enum_Status;
       |  signal bitClkCnt      : unsigned(clog2(BIT_CLOCKS) - 1 downto 0);
       |  signal dataBitCnt     : unsigned(2 downto 0);
       |  signal shiftData      : std_logic_vector(7 downto 0);
       |  signal tx_din         : std_logic;
       |  signal tx_en_din      : std_logic;
       |  signal tx_done_din    : std_logic;
       |  signal status_din     : t_enum_Status;
       |  signal bitClkCnt_din  : unsigned(clog2(BIT_CLOCKS) - 1 downto 0);
       |  signal dataBitCnt_din : unsigned(2 downto 0);
       |  signal shiftData_din  : std_logic_vector(7 downto 0);
       |begin
       |  process (all)
       |  begin
       |    tx_din                 <= tx;
       |    tx_en_din              <= tx_en;
       |    tx_done_din            <= tx_done;
       |    status_din             <= status;
       |    bitClkCnt_din          <= bitClkCnt;
       |    dataBitCnt_din         <= dataBitCnt;
       |    shiftData_din          <= shiftData;
       |    case status is
       |      when Status_Idle     =>
       |        tx_en_din          <= '0';
       |        tx_din             <= '1';
       |        tx_done_din        <= '0';
       |        bitClkCnt_din      <= resize(d"0", clog2(BIT_CLOCKS));
       |        dataBitCnt_din     <= 3d"0";
       |        if data_en then
       |          shiftData_din    <= data;
       |          status_din       <= Status_StartBit;
       |        end if;
       |      when Status_StartBit =>
       |        tx_en_din          <= '1';
       |        tx_din             <= '0';
       |        if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |          bitClkCnt_din    <= resize(d"0", clog2(BIT_CLOCKS));
       |          status_din       <= Status_DataBits;
       |        else bitClkCnt_din <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |        end if;
       |      when Status_DataBits =>
       |        tx_din             <= shiftData(0);
       |        if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |          bitClkCnt_din    <= resize(d"0", clog2(BIT_CLOCKS));
       |          shiftData_din    <= slv_srl(shiftData, 1);
       |          if dataBitCnt = 3d"7" then
       |            dataBitCnt_din <= 3d"0";
       |            status_din     <= Status_StopBit;
       |          else dataBitCnt_din <= dataBitCnt + 3d"1";
       |          end if;
       |        else bitClkCnt_din <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |        end if;
       |      when Status_StopBit  =>
       |        tx_din             <= '1';
       |        if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
       |          bitClkCnt_din    <= resize(d"0", clog2(BIT_CLOCKS));
       |          tx_done_din      <= '1';
       |          status_din       <= Status_Finalize;
       |        else bitClkCnt_din <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
       |        end if;
       |      when Status_Finalize =>
       |        tx_en_din          <= '0';
       |        tx_done_din        <= '1';
       |        status_din         <= Status_Idle;
       |    end case;
       |  end process;
       |  process (clk)
       |  begin
       |    if rising_edge(clk) then
       |      if rst = '1' then
       |        status             <= Status_Idle;
       |        bitClkCnt          <= resize(d"0", clog2(BIT_CLOCKS));
       |        dataBitCnt         <= 3d"0";
       |      else
       |        tx                 <= tx_din;
       |        tx_en              <= tx_en_din;
       |        tx_done            <= tx_done_din;
       |        status             <= status_din;
       |        bitClkCnt          <= bitClkCnt_din;
       |        dataBitCnt         <= dataBitCnt_din;
       |        shiftData          <= shiftData_din;
       |      end if;
       |    end if;
       |  end process;
       |end UART_Tx_arch;""".stripMargin
end UART_TxSpec
