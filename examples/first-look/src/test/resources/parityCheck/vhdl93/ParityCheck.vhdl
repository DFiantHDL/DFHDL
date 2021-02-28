library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ParityCheck_pkg.all;

entity ParityCheck is
port (
  clk                    : in  std_logic;
  rst                    : in  std_logic;
  seqIn                  : in  std_logic;
  detOut                 : out std_logic
);
end ParityCheck;

architecture ParityCheck_arch of ParityCheck is  
  type E_fsm_states is (
    E_fsm_states_Even,
    E_fsm_states_Odd
  );
  signal fsm_state_prev1 : E_fsm_states := E_fsm_states_Even;
  signal fsm_state_sig   : E_fsm_states;
begin
  async_proc : process (clk, fsm_state_prev1, seqIn, rst)  
    variable fsm_state   : E_fsm_states;
  begin
    fsm_state            := fsm_state_prev1;
    case fsm_state is
      when E_fsm_states_Even =>
        detOut           <= '1';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_Odd;
        end if;
      when E_fsm_states_Odd =>
        detOut           <= '0';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_Even;
        end if;
    end case;
    fsm_state_sig        <= fsm_state;
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      fsm_state_prev1    <= E_fsm_states_Even;
    elsif rising_edge(clk) then
      fsm_state_prev1    <= fsm_state_sig;
    end if;
  end process;
end ParityCheck_arch;