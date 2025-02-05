library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;
use work.UART_Tx_pkg.all;

entity UART_Tx is
generic (
  CLK_FREQ_KHz : integer := 50000;
  BAUD_RATE_BPS : integer := 115200
);
port (
  clk     : in  std_logic;
  rst     : in  std_logic;
  data_en : in  std_logic;
  data    : in  std_logic_vector(7 downto 0);
  tx      : out std_logic;
  tx_en   : out std_logic;
  tx_done : out std_logic
);
end UART_Tx;

architecture UART_Tx_arch of UART_Tx is
  constant BIT_CLOCKS : integer := (CLK_FREQ_KHz * 1000) / BAUD_RATE_BPS;
  type t_enum_Status is (
    Status_Idle, Status_StartBit, Status_DataBits, Status_StopBit, Status_Finalize
  );
  signal status       : t_enum_Status;
  signal bitClkCnt    : unsigned(clog2(BIT_CLOCKS) - 1 downto 0);
  signal dataBitCnt   : unsigned(2 downto 0);
  signal shiftData    : std_logic_vector(7 downto 0);
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        status             <= Status_Idle;
        bitClkCnt          <= resize(d"0", clog2(BIT_CLOCKS));
        dataBitCnt         <= 3d"0";
      else
        case status is
          when Status_Idle     =>
            tx_en          <= '0';
            tx             <= '1';
            tx_done        <= '0';
            bitClkCnt      <= resize(d"0", clog2(BIT_CLOCKS));
            dataBitCnt     <= 3d"0";
            if data_en then
              shiftData    <= data;
              status       <= Status_StartBit;
            end if;
          when Status_StartBit =>
            tx_en          <= '1';
            tx             <= '0';
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
              status       <= Status_DataBits;
            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
            end if;
          when Status_DataBits =>
            tx             <= shiftData(0);
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
              shiftData    <= slv_srl(shiftData, 1);
              if dataBitCnt = 3d"7" then
                dataBitCnt <= 3d"0";
                status     <= Status_StopBit;
              else dataBitCnt <= dataBitCnt + 3d"1";
              end if;
            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
            end if;
          when Status_StopBit  =>
            tx             <= '1';
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(d"0", clog2(BIT_CLOCKS));
              tx_done      <= '1';
              status       <= Status_Finalize;
            else bitClkCnt <= bitClkCnt + resize(d"1", clog2(BIT_CLOCKS));
            end if;
          when Status_Finalize =>
            tx_en          <= '0';
            tx_done        <= '1';
            status         <= Status_Idle;
        end case;
      end if;
    end if;
  end process;
end UART_Tx_arch;
