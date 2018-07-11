---
typora-copy-images-to: graphics
---

# Connectivity

![1531312790115](graphics/1531312790115.png)TODO: Add Legend



Semantics:

* Differences between `:=` and `<>` 
  * Initialization is not copied to destination with `:=`, but it does with `<>`
  * `<>` ordering does not matter! The ordering is determined by the dependency detected when the design is flattened.
  * Input ports do not accept `:=` under any condition.
  * Output ports can accept `:=` only at the design level, but not at owner level.
  * All ports accept `<>`, but various restriction are applied, depending on the hierarchy difference, called scope and port directions.
  * `:=` is directional (consumer := producer) while `<>` set the direction automatically.



---

```scala
trait IODesign extends DFDesign {
  val i = DFUInt[8] <> IN
  val o = DFUInt[8] <> OUT
  o := i
}
```

![1531312715988](graphics/1531314030378.png)

---

```scala
trait IODesign1 extends DFDesign {
	val i = DFUInt[8] <> IN
  val o = DFUInt[8] <> OUT
  val tmp = DFUInt[8]
  tmp := i
  o := tmp
}
```

![1531313031884](graphics/1531314048642.png)

---

```scala
trait IODesign2 extends DFDesign {
  val i1 = DFUInt[8] <> IN
  val o1 = DFUInt[8] <> OUT
  val i2 = DFUInt[8] <> IN
  val o2 = DFUInt[8] <> OUT
  o1 := i1
  o2 := i2
}
```

![1531313204197](graphics/1531314259406.png)

---

```scala
trait Container extends DFDesign {
  val i = DFUInt[8] <> IN
  val o = DFUInt[8] <> OUT
  val io = new IODesign {}
  i <> io.i //Connecting between owner input and child input
  io.o <> o //Connecting between child output and owner output
}
```

![1531313619621](graphics/1531314601402.png)

---

```scala
trait Container2 extends DFDesign {
  val i = DFUInt[8] <> IN
  val o = DFUInt[8] <> OUT
  val io1 = new IODesign {}
  val io2 = new IODesign {}
  i <> io1.i     //Connecting between owner input and child input
  io1.o <> io2.i //Connecting between siblings (output <> input)
  io2.o <> o     //Connecting between child output and owner output
}
```

![1531314589019](graphics/1531314589019.png)

---

```scala
trait Container3 extends DFDesign {
  val i = DFUInt[8] <> IN
  val o = DFUInt[8] <> OUT
  val io = new IODesign2 {}
  i <> io.i1 //Connecting between owner input and child input
  i <> io.i2 //Connecting between owner input and child input
  o <> (io.o1 + io.o2)
}
```

![1531322811065](graphics/1531322880257.png)

---

