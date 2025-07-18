package AES
import dfhdl.*
given options.CompilerOptions.DropUserOpaques = true

@top class CipherSim extends RTDesign:
  val cipher = Cipher()
  val data = Bits(128) <> VAR.REG init CipherSim.initData
  val key = Bits(128) <> VAR.REG init CipherSim.initKey
  val cnt = Int <> VAR.REG init 0
  data.din := (data(126, 0), data(127))
  key.din := (key(126, 0), key(127))
  cnt.din := cnt + 1
  if cnt == CipherSim.rotations then
    finish()
  else
    assert(
      cipher.o.bits == CipherSim.expectedDataOut(cnt),
      s"cnt: $cnt, expected: ${CipherSim.expectedDataOut(cnt)}, actual: ${cipher.o.bits}"
    )

  cipher.data <> data.as(AESData)
  cipher.key <> key.as(AESKey)
end CipherSim

object CipherSim:
  val initData = h"32 43 f6 a8 88 5a 30 8d 31 31 98 a2 e0 37 07 34"
  val initKey = h"2b 7e 15 16 28 ae d2 a6 ab f7 15 88 09 cf 4f 3c"
  val rotations = 128
  val expectedDataOut = DFVector(Bits(128) X rotations)(
    h"3925841d02dc09fbdc118597196a0b32",
    h"21b1d6e7c2fcfaf4a828c86688c5d92f",
    h"0e9ec2cef5b17a66194c55e171261c91",
    h"5ed441598dfb09d796a51ae5d108b39d",
    h"702a15773ae612cda40d0a439194016e",
    h"faa7d79aec23cee3266b9c252e4fcccd",
    h"74a9a049a872900de89fe402742de8ca",
    h"bca585fa61cfb22cbd3cd96260ec2bec",
    h"b742fb040f077de8303ebfb416a3e81c",
    h"9f531a7bd80c1c351612bfe22d8913fa",
    h"b0828e36eebccfed5748b7a6c8fbcbe7",
    h"7f3805298e468228957bf6fd6dfad850",
    h"2a305b3f6e3430d64adba555bd17bc00",
    h"1333c442b919811c4dfe9ad3f90122bb",
    h"71af126d7688ea0b8c02df214f9b9fc9",
    h"b0b06d3a8a9e135f7729ee4aa2c09577",
    h"869fa249f36c1cbe872d24dd4a2b898e",
    h"dc24478035e1576dd14cd330f233ca49",
    h"090849209ff4d9bdfb0318207eefc016",
    h"45ef1cd7f16d83964c2dbbfbf6094c86",
    h"9abe16b464fc2037528ea5de0e756896",
    h"a0f86c4981685d52078970cd21e3ffc7",
    h"e6569d33d21637d1a556ba931d6b58fc",
    h"70fb69a9e9e7dcb531458fb8d444081f",
    h"f04440f17157d6a1d42bdb3c82458f05",
    h"95de74e2cff9555703ea736e3a41c413",
    h"6fdace8af86bdc26d3ae8e34d42c8d47",
    h"22c884a2896fce844bdb4e1c40538ea7",
    h"ea1db32bac832341e4057fda2044db69",
    h"0f925c34ae5d4d9fbc961faf7260eae0",
    h"0d7ec8dc7981cfb9dc81f3d8470189f2",
    h"92b199ee9cb44af895606d0617a4f050",
    h"e6a25d317435b0dd59a9c2e17373ed8a",
    h"fcfd9935f3f0e60fc076b8a1cd5618aa",
    h"c8c8d3664e5f1648e0298e1695f920d6",
    h"afe540fc2ea301f5ed128f4baa52d74e",
    h"e4d113eb5783de01dab22324ea45f065",
    h"dfa9d12c3a4722c5025ddbd34360c04c",
    h"419cfab35eba93520d3750e78ccbd34f",
    h"2128fede3b9e50e76d5dbc08c415a434",
    h"be7e743397b1d90bb9e5ead9953f899e",
    h"f09bde3e012f5a3d29cff5fe267d1950",
    h"076b6bf7abd80567dc4a50f641768bf8",
    h"7f039373e6f23517392bbe8f70f679c4",
    h"286c169da527d026be44b626dfed5ee9",
    h"3d4002318c55c25c9e20d24d21e84604",
    h"d5842c769f563b44b74d80dd670b2a2e",
    h"cc03ea3ca1e45996c3425197ab1fb2b5",
    h"72924c4d7cb32ff65ab7342b5d9aeaf5",
    h"e2de6785a17ce3cd894e3afcbf8d8a72",
    h"cb6c8c67fa7b2ea912d2187da1643269",
    h"bd456e180e9f8493e0656de8a5691332",
    h"0db52a689a52ca9beac669fe24d62174",
    h"cd2b2a21d2a9862c885061c9f4166c97",
    h"9b15319495f8e91c522c53864f0d5160",
    h"c35c1c03370ca1961cf758f0195a56cd",
    h"5d9fc83cf9a97091b5a93ad95480f7dc",
    h"c11474aa16465e088dfa322410b105a7",
    h"a5f87c2adcc297a87845fcd1639d6995",
    h"194b7466c4234e55c925be7f5626d6f1",
    h"a8a383ad0ec3842dd45f5808bce0a58a",
    h"d31f7874304b573ad1a8876a173c804e",
    h"4a57ad1148f804ebf13fdba83e988a25",
    h"157cc204047e6a61e19cbb885ff18f0a",
    h"5989b4f685a23e12f4887545871c70a7",
    h"8baf68ba62a24b30ac63d19fcc79da27",
    h"a623872c9032b01cdb01d09fbe4207be",
    h"c848f583085689e2e95f9fe4b7ee08d7",
    h"e114fbf4740a6ea81a43463480ab158e",
    h"22e3aa6fa16357fb93586932f45d3fdd",
    h"b7871a5bf6e8b97c842e096a0a766b9d",
    h"9e5c83e9e4a513d0bd5e8ccfb98b2b6e",
    h"fe8068b5b0dd8c611bb2971f32aaed56",
    h"69b8c78895cb57845721f3951a2f1dd4",
    h"ab004eb15fce771746325fb40e7fcd59",
    h"f736640509456fc0032d31faa5544de8",
    h"7b587112fe4cfb4f3d8921f7d7b0bb75",
    h"31c8a695f3731a5ff94142d9fffc5037",
    h"382e0031862aca76893501cc11e86945",
    h"55bb48c9caef5c59eaca6b4bc0bb6d15",
    h"11e80baf6451211c6633e3a1f59bac7c",
    h"d60d65bef59cb257c996b0c97720f848",
    h"459596e7c0468e0398733f2ebac1f745",
    h"6d6c3bda5eb58da6294b3e26cc9a99fb",
    h"054fb0a5726bc37af4f4817f260e1efb",
    h"ed0e75e9dee79919a33b17f8ac7692bb",
    h"e186fbb068100618ed20b85272d74da1",
    h"1ef3c573e1e7f78af8d1af3bb8c75487",
    h"82ff1dddedc2d92cc1f75dc45ed2fa79",
    h"4e5fac1cf288c4971215016dc54be198",
    h"3a647e73352c0eb2b4fb6ba42a1b02fb",
    h"205f37b9d824fdcfd3d1d42d63ade8c9",
    h"98cf08028c68015a28785d45c0f6f6f4",
    h"c0fdc3d612778207f9d46b0b1fea2b15",
    h"95259f05dd2b23663a5efa689e533783",
    h"b4154111ab10be49f31967ccf1000923",
    h"44fc9d735c8ad1705416f14940135bf7",
    h"1ec6f07dbdeae5ed950ad3aba8225f4e",
    h"b57aca9bcfe054d903ad0369b77caa09",
    h"29ef8f9166c42a2c00343bc2dbcf063c",
    h"1ae7cb93d988168ee62180184db4bf6e",
    h"490c4244f1384542f067465b0a3ce8a7",
    h"e645c608795b5c360eb33198fa424054",
    h"146af2f91555564c44b8bddd0b5598d3",
    h"52a2c7d4129d3ef547abe81753b8ab0a",
    h"dd94bf9e4c6388b3322c99942df0e0a2",
    h"717efe25d9fdfaaedbd6d9864b96aab0",
    h"179b24076637184cceaaeb3758f41a79",
    h"6ef6d74227ea0fef0106261751cd2dd0",
    h"882c639280f4169069fa920c3a689870",
    h"5b9ea8e32bebab158c32e796f4017d93",
    h"a45ac5ed79f8cbe683f48a5b49f84382",
    h"fe8535a4f1cde211c5d0fd9da8abe889",
    h"71d8c2e8df5a59547c3563cbc11473ab",
    h"258e81dcbd1abfed829598deecda32df",
    h"6db70a8de77425d00a3e5a7a978f64f2",
    h"5b3cf9ec76cc8f2e5cb7d50300b77d5b",
    h"6c0e603f2279bc5d4ee3a1d838e1eeea",
    h"1af7d5326d50bcc19b62d64531e2db5a",
    h"3810e3e8ff7493f84a3a669590f29b63",
    h"3118eb802e8b594c2b50b3556aaed739",
    h"d2506a40b102df00ac96e436d8a9d411",
    h"f5d3f27ff52ca9121b2af2eba5b6eaaa",
    h"f4e83d8fb4349951d80d129e3fa9a617",
    h"87d61b2b60ac4e2892aae2c9f5ed8a0b",
    h"ed366c00ebc3fc18a0b73c605defabad",
    h"caea999a7f651b8cc7b05a6705cee530",
    h"e08681e1e4b64d14733b624e4e8c250b"
  )
end CipherSim
