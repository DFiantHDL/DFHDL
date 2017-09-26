entity fib is
  port (
    CLK   : IN  std_logic;
    DATA  : OUT unsigned(63 downto 0);
    REQ   : IN  std_logic
  );
end fib;

architecture fib_arch of fib is
  signal DATA_out  : unsigned(63 downto 0);
  signal out_prev  : unsigned(63 downto 0);
  signal out_prev2 : unsigned(63 downto 0);
  type state_type is (Out0, Out1, OutOthers);
  signal state : state_type := Out0;

begin
  DATA <= DATA_out;
  process(CLK) is
  begin
    if REQ = '1' then
    begin
      case state is
        when Out0 =>
          DATA_out <= (others => '0');
          state <= Out1;
        when Out1 =>
          DATA_out <= (0 => '1', others => '0');
          state <= OutOthers;
        when OutOthers =>
          DATA_out <= out_prev + out_prev2;
      end case;
      out_prev2 <= out_prev;
      out_prev <= DATA_out;
    end if;
  end process;
end fib_arch;