// format: off
package issues.i118

import dfhdl.*

@top(false) class ShiftIssue() extends RTDesign:  
    val bitvec = Bits(10) <> VAR
    val bitvec2 = Bits(10) <> OUT
    val bitvec3 = Bits(10) <> VAR
    
    val signed1 = SInt(10) <> VAR
    val signed2 = SInt(10) <> OUT
    val signed3 = SInt(10) <> OUT
    
    val unsigned1 = UInt(10) <> VAR
    val unsigned2 = UInt(10) <> OUT
    val unsigned3 = UInt(10) <> OUT

    bitvec2 := bitvec >> 1 // not ok, uses sla instead of srl
    bitvec3 := bitvec << 1 // ok
    
    signed2 := signed1 >> 1 // not ok, converts value to unsigned before applying sra
    signed3 := signed1 << 1 // ok

    unsigned2 := unsigned1 >> 1
    unsigned3 := unsigned1 << 1
end ShiftIssue
