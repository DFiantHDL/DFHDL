library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.SeqDet_pkg.all;

entity SeqDet is
port (
  clk                       : in  std_logic;
  rst                       : in  std_logic;
  seqIn                     : in  boolean;
  detOut                    : out boolean
);
end SeqDet;

architecture SeqDet_arch of SeqDet is  
  type E_detFsm_states is (E_detFsm_states_S1001, E_detFsm_states_S1, E_detFsm_states_S0, E_detFsm_states_S100, E_detFsm_states_S10);
  signal detFsm_state_prev1 : E_detFsm_states := E_detFsm_states_S0;
  signal detFsm_state_sig   : E_detFsm_states;
begin
  async_proc : process (clk, detFsm_state_prev1, seqIn, rst)  
    variable detFsm_state   : E_detFsm_states := E_detFsm_states_S0;
  begin
    detFsm_state            := detFsm_state_prev1;
    case detFsm_state is
      when E_detFsm_states_S0 =>
        detOut              <= false;
        if seqIn then
          detFsm_state      := E_detFsm_states_S1;
        else
          detFsm_state      := E_detFsm_states_S0;
        end if;
      when E_detFsm_states_S1 =>
        detOut              <= false;
        if seqIn then
          detFsm_state      := E_detFsm_states_S1;
        else
          detFsm_state      := E_detFsm_states_S10;
        end if;
      when E_detFsm_states_S10 =>
        detOut              <= false;
        if seqIn then
          detFsm_state      := E_detFsm_states_S1;
        else
          detFsm_state      := E_detFsm_states_S100;
        end if;
      when E_detFsm_states_S100 =>
        detOut              <= false;
        if seqIn then
          detFsm_state      := E_detFsm_states_S1001;
        else
          detFsm_state      := E_detFsm_states_S0;
        end if;
      when E_detFsm_states_S1001 =>
        detOut              <= true;
        if seqIn then
          detFsm_state      := E_detFsm_states_S1;
        else
          detFsm_state      := E_detFsm_states_S10;
        end if;
    end case;
    detFsm_state_sig        <= detFsm_state;
  end process;
  sync_proc : process (rst, clk)
  begin
    if rst = '0' then
      detFsm_state_prev1    <= E_detFsm_states_S0;
    elsif rising_edge(clk) then
      detFsm_state_prev1    <= detFsm_state_sig;
    end if;
  end process;
end SeqDet_arch;