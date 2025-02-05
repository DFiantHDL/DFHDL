library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dfhdl_pkg.all;

package CipherNoOpaques_pkg is
type t_arrX1_std_logic_vector is array (natural range <>) of std_logic_vector;
function bitWidth(A : t_arrX1_std_logic_vector) return integer;
function to_slv(A : t_arrX1_std_logic_vector) return std_logic_vector;
function to_t_arrX1_std_logic_vector(A : std_logic_vector; D1 : integer; D0 : integer) return t_arrX1_std_logic_vector;
function bool_sel(C : boolean; T : t_arrX1_std_logic_vector; F : t_arrX1_std_logic_vector) return t_arrX1_std_logic_vector;
subtype t_opaque_AESByte is std_logic_vector(7 downto 0);
function to_t_opaque_AESByte(A: std_logic_vector) return t_opaque_AESByte;
type t_arrX1_t_opaque_AESByte is array (natural range <>) of t_opaque_AESByte;
function bitWidth(A : t_arrX1_t_opaque_AESByte) return integer;
function to_slv(A : t_arrX1_t_opaque_AESByte) return std_logic_vector;
function to_t_arrX1_t_opaque_AESByte(A : std_logic_vector; D1 : integer) return t_arrX1_t_opaque_AESByte;
function bool_sel(C : boolean; T : t_arrX1_t_opaque_AESByte; F : t_arrX1_t_opaque_AESByte) return t_arrX1_t_opaque_AESByte;
subtype t_opaque_AESWord is t_arrX1_t_opaque_AESByte(0 to 3);
function to_t_opaque_AESWord(A: std_logic_vector) return t_opaque_AESWord;
type t_arrX1_t_opaque_AESWord is array (natural range <>) of t_opaque_AESWord;
function bitWidth(A : t_arrX1_t_opaque_AESWord) return integer;
function to_slv(A : t_arrX1_t_opaque_AESWord) return std_logic_vector;
function to_t_arrX1_t_opaque_AESWord(A : std_logic_vector; D1 : integer) return t_arrX1_t_opaque_AESWord;
function bool_sel(C : boolean; T : t_arrX1_t_opaque_AESWord; F : t_arrX1_t_opaque_AESWord) return t_arrX1_t_opaque_AESWord;
subtype t_opaque_AESKey is t_arrX1_t_opaque_AESWord(0 to 3);
function to_t_opaque_AESKey(A: std_logic_vector) return t_opaque_AESKey;
subtype t_opaque_AESData is t_arrX1_t_opaque_AESWord(0 to 3);
function to_t_opaque_AESData(A: std_logic_vector) return t_opaque_AESData;
subtype t_opaque_AESKeySchedule is t_arrX1_t_opaque_AESWord(0 to 43);
function to_t_opaque_AESKeySchedule(A: std_logic_vector) return t_opaque_AESKeySchedule;
subtype t_opaque_AESState is t_arrX1_t_opaque_AESWord(0 to 3);
function to_t_opaque_AESState(A: std_logic_vector) return t_opaque_AESState;
subtype t_opaque_AESRoundKey is t_arrX1_t_opaque_AESWord(0 to 3);
function to_t_opaque_AESRoundKey(A: std_logic_vector) return t_opaque_AESRoundKey;

constant sboxLookupTable : t_arrX1_std_logic_vector(0 to 255)(7 downto 0) := (
    0  => x"63",   1 => x"7c",   2 => x"77",   3 => x"7b",   4 => x"f2",   5 => x"6b",   6 => x"6f",   7 => x"c5",
    8  => x"30",   9 => x"01",  10 => x"67",  11 => x"2b",  12 => x"fe",  13 => x"d7",  14 => x"ab",  15 => x"76",
   16  => x"ca",  17 => x"82",  18 => x"c9",  19 => x"7d",  20 => x"fa",  21 => x"59",  22 => x"47",  23 => x"f0",
   24  => x"ad",  25 => x"d4",  26 => x"a2",  27 => x"af",  28 => x"9c",  29 => x"a4",  30 => x"72",  31 => x"c0",
   32  => x"b7",  33 => x"fd",  34 => x"93",  35 => x"26",  36 => x"36",  37 => x"3f",  38 => x"f7",  39 => x"cc",
   40  => x"34",  41 => x"a5",  42 => x"e5",  43 => x"f1",  44 => x"71",  45 => x"d8",  46 => x"31",  47 => x"15",
   48  => x"04",  49 => x"c7",  50 => x"23",  51 => x"c3",  52 => x"18",  53 => x"96",  54 => x"05",  55 => x"9a",
   56  => x"07",  57 => x"12",  58 => x"80",  59 => x"e2",  60 => x"eb",  61 => x"27",  62 => x"b2",  63 => x"75",
   64  => x"09",  65 => x"83",  66 => x"2c",  67 => x"1a",  68 => x"1b",  69 => x"6e",  70 => x"5a",  71 => x"a0",
   72  => x"52",  73 => x"3b",  74 => x"d6",  75 => x"b3",  76 => x"29",  77 => x"e3",  78 => x"2f",  79 => x"84",
   80  => x"53",  81 => x"d1",  82 => x"00",  83 => x"ed",  84 => x"20",  85 => x"fc",  86 => x"b1",  87 => x"5b",
   88  => x"6a",  89 => x"cb",  90 => x"be",  91 => x"39",  92 => x"4a",  93 => x"4c",  94 => x"58",  95 => x"cf",
   96  => x"d0",  97 => x"ef",  98 => x"aa",  99 => x"fb", 100 => x"43", 101 => x"4d", 102 => x"33", 103 => x"85",
  104  => x"45", 105 => x"f9", 106 => x"02", 107 => x"7f", 108 => x"50", 109 => x"3c", 110 => x"9f", 111 => x"a8",
  112  => x"51", 113 => x"a3", 114 => x"40", 115 => x"8f", 116 => x"92", 117 => x"9d", 118 => x"38", 119 => x"f5",
  120  => x"bc", 121 => x"b6", 122 => x"da", 123 => x"21", 124 => x"10", 125 => x"ff", 126 => x"f3", 127 => x"d2",
  128  => x"cd", 129 => x"0c", 130 => x"13", 131 => x"ec", 132 => x"5f", 133 => x"97", 134 => x"44", 135 => x"17",
  136  => x"c4", 137 => x"a7", 138 => x"7e", 139 => x"3d", 140 => x"64", 141 => x"5d", 142 => x"19", 143 => x"73",
  144  => x"60", 145 => x"81", 146 => x"4f", 147 => x"dc", 148 => x"22", 149 => x"2a", 150 => x"90", 151 => x"88",
  152  => x"46", 153 => x"ee", 154 => x"b8", 155 => x"14", 156 => x"de", 157 => x"5e", 158 => x"0b", 159 => x"db",
  160  => x"e0", 161 => x"32", 162 => x"3a", 163 => x"0a", 164 => x"49", 165 => x"06", 166 => x"24", 167 => x"5c",
  168  => x"c2", 169 => x"d3", 170 => x"ac", 171 => x"62", 172 => x"91", 173 => x"95", 174 => x"e4", 175 => x"79",
  176  => x"e7", 177 => x"c8", 178 => x"37", 179 => x"6d", 180 => x"8d", 181 => x"d5", 182 => x"4e", 183 => x"a9",
  184  => x"6c", 185 => x"56", 186 => x"f4", 187 => x"ea", 188 => x"65", 189 => x"7a", 190 => x"ae", 191 => x"08",
  192  => x"ba", 193 => x"78", 194 => x"25", 195 => x"2e", 196 => x"1c", 197 => x"a6", 198 => x"b4", 199 => x"c6",
  200  => x"e8", 201 => x"dd", 202 => x"74", 203 => x"1f", 204 => x"4b", 205 => x"bd", 206 => x"8b", 207 => x"8a",
  208  => x"70", 209 => x"3e", 210 => x"b5", 211 => x"66", 212 => x"48", 213 => x"03", 214 => x"f6", 215 => x"0e",
  216  => x"61", 217 => x"35", 218 => x"57", 219 => x"b9", 220 => x"86", 221 => x"c1", 222 => x"1d", 223 => x"9e",
  224  => x"e1", 225 => x"f8", 226 => x"98", 227 => x"11", 228 => x"69", 229 => x"d9", 230 => x"8e", 231 => x"94",
  232  => x"9b", 233 => x"1e", 234 => x"87", 235 => x"e9", 236 => x"ce", 237 => x"55", 238 => x"28", 239 => x"df",
  240  => x"8c", 241 => x"a1", 242 => x"89", 243 => x"0d", 244 => x"bf", 245 => x"e6", 246 => x"42", 247 => x"68",
  248  => x"41", 249 => x"99", 250 => x"2d", 251 => x"0f", 252 => x"b0", 253 => x"54", 254 => x"bb", 255 => x"16"
);
constant Rcon            : t_arrX1_t_opaque_AESWord(0 to 10) := (
   0   => (0 => x"00", 1 => x"00", 2 => x"00", 3 => x"00"),
   1   => (0 => x"01", 1 => x"00", 2 => x"00", 3 => x"00"),
   2   => (0 => x"02", 1 => x"00", 2 => x"00", 3 => x"00"),
   3   => (0 => x"04", 1 => x"00", 2 => x"00", 3 => x"00"),
   4   => (0 => x"08", 1 => x"00", 2 => x"00", 3 => x"00"),
   5   => (0 => x"10", 1 => x"00", 2 => x"00", 3 => x"00"),
   6   => (0 => x"20", 1 => x"00", 2 => x"00", 3 => x"00"),
   7   => (0 => x"40", 1 => x"00", 2 => x"00", 3 => x"00"),
   8   => (0 => x"80", 1 => x"00", 2 => x"00", 3 => x"00"),
   9   => (0 => x"1b", 1 => x"00", 2 => x"00", 3 => x"00"),
  10   => (0 => x"36", 1 => x"00", 2 => x"00", 3 => x"00")
);
end package CipherNoOpaques_pkg;

package body CipherNoOpaques_pkg is
function to_t_opaque_AESByte(A : std_logic_vector) return t_opaque_AESByte is
begin
  return A;
end;
function to_t_opaque_AESWord(A : std_logic_vector) return t_opaque_AESWord is
begin
  return to_t_arrX1_t_opaque_AESByte(A, 4);
end;
function to_t_opaque_AESKey(A : std_logic_vector) return t_opaque_AESKey is
begin
  return to_t_arrX1_t_opaque_AESWord(A, 4);
end;
function to_t_opaque_AESData(A : std_logic_vector) return t_opaque_AESData is
begin
  return to_t_arrX1_t_opaque_AESWord(A, 4);
end;
function to_t_opaque_AESKeySchedule(A : std_logic_vector) return t_opaque_AESKeySchedule is
begin
  return to_t_arrX1_t_opaque_AESWord(A, 44);
end;
function to_t_opaque_AESState(A : std_logic_vector) return t_opaque_AESState is
begin
  return to_t_arrX1_t_opaque_AESWord(A, 4);
end;
function to_t_opaque_AESRoundKey(A : std_logic_vector) return t_opaque_AESRoundKey is
begin
  return to_t_arrX1_t_opaque_AESWord(A, 4);
end;
function bitWidth(A : t_arrX1_std_logic_vector) return integer is
begin
  return A'length * bitWidth(A(0));
end;
function to_slv(A : t_arrX1_std_logic_vector) return std_logic_vector is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : std_logic_vector(bitWidth(A) - 1 downto 0);
begin
  cellBitWidth := bitWidth(A(0));
  lo   := bitWidth(A);
  for i in 0 to A'length-1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(hi downto lo) := A(i);
  end loop;
  return ret;
end;
function to_t_arrX1_std_logic_vector(A : std_logic_vector; D1 : integer; D0 : integer) return t_arrX1_std_logic_vector is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : t_arrX1_std_logic_vector(0 to D1 - 1)(D0 - 1 downto 0);
begin
  cellBitWidth := bitWidth(ret(0));
  lo   := A'length;
  for i in 0 to ret'length - 1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(i) := A(hi downto lo);
  end loop;
  return ret;
end;
function bool_sel(C : boolean; T : t_arrX1_std_logic_vector; F : t_arrX1_std_logic_vector) return t_arrX1_std_logic_vector is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bitWidth(A : t_arrX1_t_opaque_AESByte) return integer is
begin
  return A'length * bitWidth(A(0));
end;
function to_slv(A : t_arrX1_t_opaque_AESByte) return std_logic_vector is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : std_logic_vector(bitWidth(A) - 1 downto 0);
begin
  cellBitWidth := bitWidth(A(0));
  lo   := bitWidth(A);
  for i in 0 to A'length-1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(hi downto lo) := A(i);
  end loop;
  return ret;
end;
function to_t_arrX1_t_opaque_AESByte(A : std_logic_vector; D1 : integer) return t_arrX1_t_opaque_AESByte is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : t_arrX1_t_opaque_AESByte(0 to D1 - 1);
begin
  cellBitWidth := bitWidth(ret(0));
  lo   := A'length;
  for i in 0 to ret'length - 1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(i) := to_t_opaque_AESByte(A(hi downto lo));
  end loop;
  return ret;
end;
function bool_sel(C : boolean; T : t_arrX1_t_opaque_AESByte; F : t_arrX1_t_opaque_AESByte) return t_arrX1_t_opaque_AESByte is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;
function bitWidth(A : t_arrX1_t_opaque_AESWord) return integer is
begin
  return A'length * bitWidth(A(0));
end;
function to_slv(A : t_arrX1_t_opaque_AESWord) return std_logic_vector is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : std_logic_vector(bitWidth(A) - 1 downto 0);
begin
  cellBitWidth := bitWidth(A(0));
  lo   := bitWidth(A);
  for i in 0 to A'length-1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(hi downto lo) := to_slv(A(i));
  end loop;
  return ret;
end;
function to_t_arrX1_t_opaque_AESWord(A : std_logic_vector; D1 : integer) return t_arrX1_t_opaque_AESWord is
  variable hi            : integer;
  variable lo            : integer;
  variable cellBitWidth  : integer;
  variable ret           : t_arrX1_t_opaque_AESWord(0 to D1 - 1);
begin
  cellBitWidth := bitWidth(ret(0));
  lo   := A'length;
  for i in 0 to ret'length - 1 loop
    hi := lo - 1; lo := hi - cellBitWidth + 1;
    ret(i) := to_t_opaque_AESWord(A(hi downto lo));
  end loop;
  return ret;
end;
function bool_sel(C : boolean; T : t_arrX1_t_opaque_AESWord; F : t_arrX1_t_opaque_AESWord) return t_arrX1_t_opaque_AESWord is
begin
  if C then
    return T;
  else
    return F;
  end if;
end;

end package body CipherNoOpaques_pkg;
