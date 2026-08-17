library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;

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
  type Status is (
    Status_Idle, Status_StartBit, Status_DataBits, Status_StopBit, Status_Finalize
  );
  signal status_0     : Status;
  signal bitClkCnt    : unsigned(clog2(BIT_CLOCKS) - 1 downto 0);
  signal dataBitCnt   : unsigned(2 downto 0);
  signal shiftData    : std_logic_vector(7 downto 0);
begin
  constraint_0: assert (BIT_CLOCKS - 1) >= 0
    report "Design parameter violation found. Expected: (BIT_CLOCKS - 1) >= 0" severity FAILURE;
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        status_0           <= Status_Idle;
        bitClkCnt          <= resize(to_unsigned(0, 1), clog2(BIT_CLOCKS));
        dataBitCnt         <= to_unsigned(0, 3);
      else
        case status_0 is
          when Status_Idle     =>
            tx_en          <= '0';
            tx             <= '1';
            tx_done        <= '0';
            bitClkCnt      <= resize(to_unsigned(0, 1), clog2(BIT_CLOCKS));
            dataBitCnt     <= to_unsigned(0, 3);
            if to_bool(data_en) then
              shiftData    <= data;
              status_0     <= Status_StartBit;
            end if;
          when Status_StartBit =>
            tx_en          <= '1';
            tx             <= '0';
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(to_unsigned(0, 1), clog2(BIT_CLOCKS));
              status_0     <= Status_DataBits;
            else bitClkCnt <= bitClkCnt + resize(to_unsigned(1, 1), clog2(BIT_CLOCKS));
            end if;
          when Status_DataBits =>
            tx             <= shiftData(0);
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(to_unsigned(0, 1), clog2(BIT_CLOCKS));
              shiftData    <= slv_srl(shiftData, 1);
              if dataBitCnt = to_unsigned(7, 3) then
                dataBitCnt <= to_unsigned(0, 3);
                status_0   <= Status_StopBit;
              else dataBitCnt <= dataBitCnt + to_unsigned(1, 3);
              end if;
            else bitClkCnt <= bitClkCnt + resize(to_unsigned(1, 1), clog2(BIT_CLOCKS));
            end if;
          when Status_StopBit  =>
            tx             <= '1';
            if bitClkCnt = to_unsigned(BIT_CLOCKS - 1, clog2(BIT_CLOCKS)) then
              bitClkCnt    <= resize(to_unsigned(0, 1), clog2(BIT_CLOCKS));
              tx_done      <= '1';
              status_0     <= Status_Finalize;
            else bitClkCnt <= bitClkCnt + resize(to_unsigned(1, 1), clog2(BIT_CLOCKS));
            end if;
          when Status_Finalize =>
            tx_en          <= '0';
            tx_done        <= '1';
            status_0       <= Status_Idle;
        end case;
      end if;
    end if;
  end process;
end UART_Tx_arch;
