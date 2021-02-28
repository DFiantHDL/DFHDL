library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SeqDet_pkg.all;

entity SeqDet is
port (
  clk                    : in  std_logic;
  rst                    : in  std_logic;
  seqIn                  : in  std_logic;
  detOut                 : out std_logic
);
end SeqDet;

architecture SeqDet_arch of SeqDet is  
  type E_fsm_states is (
    E_fsm_states_S0,
    E_fsm_states_S1,
    E_fsm_states_S10,
    E_fsm_states_S100,
    E_fsm_states_S1001
  );
  signal fsm_state_prev1 : E_fsm_states := E_fsm_states_S0;
  signal fsm_state_sig   : E_fsm_states;
begin
  async_proc : process (all)  
    variable fsm_state   : E_fsm_states;
  begin
    fsm_state            := fsm_state_prev1;
    case fsm_state is
      when E_fsm_states_S0 =>
        detOut           <= '0';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_S1;
        else
          fsm_state      := E_fsm_states_S0;
        end if;
      when E_fsm_states_S1 =>
        detOut           <= '0';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_S1;
        else
          fsm_state      := E_fsm_states_S10;
        end if;
      when E_fsm_states_S10 =>
        detOut           <= '0';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_S1;
        else
          fsm_state      := E_fsm_states_S100;
        end if;
      when E_fsm_states_S100 =>
        detOut           <= '0';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_S1001;
        else
          fsm_state      := E_fsm_states_S0;
        end if;
      when E_fsm_states_S1001 =>
        detOut           <= '1';
        if seqIn = '1' then
          fsm_state      := E_fsm_states_S1;
        else
          fsm_state      := E_fsm_states_S10;
        end if;
    end case;
    fsm_state_sig        <= fsm_state;
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      fsm_state_prev1    <= E_fsm_states_S0;
    elsif rising_edge(clk) then
      fsm_state_prev1    <= fsm_state_sig;
    end if;
  end process;
end SeqDet_arch;