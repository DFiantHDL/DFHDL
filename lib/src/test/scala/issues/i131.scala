// format: off
package issues.i131

import dfhdl.*

@top(false) class DictControl(
    val fetch_count : Int <> CONST,  // set to 2
    val dict_entry_size : Int = 20
) extends RTDesign:
    val dict_in = Bits(fetch_count * dict_entry_size) <> IN 
    
    val matching = Bits(fetch_count) <> VAR
    val addr_r = Bits(12) <> VAR.REG init all(0)
    val idx_r = Bits(12) <> VAR.REG init all(0)
    val sym_r = Bits(8) <> VAR.REG init all(0)
    val entry_count = UInt(12) <> VAR.REG init 0
    
    entry_count.din := 0
    sym_r.din := b"8'0"
    idx_r.din := b"12'0"
    addr_r.din := b"12'0"
    
    for (i <- 0 until fetch_count.toScalaInt)
        if ((dict_in(dict_entry_size * (i+1) - 1, dict_entry_size * i) == (idx_r, sym_r)) && (addr_r + i < entry_count - 1))
            matching(i) := 1