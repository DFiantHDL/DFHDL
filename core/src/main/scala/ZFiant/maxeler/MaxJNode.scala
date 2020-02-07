package ZFiant.maxeler
import ZFiant._

final case class MaxJNode(designDB : DFDesign.DB) {
  private val topMembers = designDB.ownerMemberTable(designDB.top)
  private val topPorts : List[DFAny.PortOf[_ <: DFAny.Type]] = topMembers.collect{
    case p : DFAny.Port.In[_,_] => p
    case p : DFAny.Port.Out[_,_] => p
  }
  private val pullInputs : List[DFAny.PortInOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.In[_,_] if p.tags.customTags.contains(MaxelerStreamIOPull) => p
  }
  private val pullOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.Out[_,_] if p.tags.customTags.contains(MaxelerStreamIOPull) => p
  }
  private val pushInputs : List[DFAny.PortInOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.In[_,_] if p.tags.customTags.contains(MaxelerStreamIOPush) => p
  }
  private val pushOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.Out[_,_] if p.tags.customTags.contains(MaxelerStreamIOPush) => p
  }
  private val scalarInputs : List[DFAny.PortInOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.In[_,_] if p.tags.customTags.contains(MaxelerScalarIO) => p
  }
  private val scalarOutputs : List[DFAny.PortOutOf[_ <: DFAny.Type]] = topPorts.collect {
    case p : DFAny.Port.Out[_,_] if p.tags.customTags.contains(MaxelerScalarIO) => p
  }

  private val pullInZ = (pullInputs, pullInputs.map(p => new MetaDesign() {
    final val data = DFAny.Port.In(p.dfType) setNamePrefix(s"${p.name}_")
    final val empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val pullOutZ = (pullOutputs, pullOutputs.map(p => new MetaDesign() {
    final val data = DFAny.Port.Out(p.dfType) setNamePrefix(s"${p.name}_")
    final val empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val almost_empty = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val read = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushInZ = (pushInputs, pushInputs.map(p => new MetaDesign() {
    final val data = DFAny.Port.In(p.dfType) setNamePrefix(s"${p.name}_")
    final val stall = DFBool() <> OUT init true setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> IN setNamePrefix(s"${p.name}_")
  })).zipped
  private val pushOutZ = (pushOutputs, pushOutputs.map(p => new MetaDesign() {
    final val data = DFAny.Port.Out(p.dfType) setNamePrefix(s"${p.name}_")
    final val stall = DFBool() <> IN setNamePrefix(s"${p.name}_")
    final val valid = DFBool() <> OUT init false setNamePrefix(s"${p.name}_")
  })).zipped
  private val scalaInZ = (scalarInputs, scalarInputs.map(p => new MetaDesign() {
    final val reg = p.prev() setNamePrefix(s"${p.name}_")
  })).zipped

  private val control = new MetaDesign() {
    val empties : List[DFBool] = pullInZ.map{case (_,d) => d.empty}
    val stalls : List[DFBool] = pushOutZ.map{case (_,d) => d.stall}

    //This forces a Join between all stream IOs. In the future we can generalize this according to
    //more enhanced constraints
    final val ready =
      empties.drop(1).foldLeft(!empties.head)((and, e) => and && !e) &&
      stalls.drop(1).foldLeft(!stalls.head.prev)((and, e) => and && !e.prev)

    pullInZ.foreach{case (_,d) => d.read := ready}
    pushOutZ.foreach{case (_,d) => d.valid := false}
    final val guard = ifdf(ready.prev){
      pushOutZ.foreach{case (_,d) => d.valid := true}
    }
  }

  val db : DFDesign.DB = {
    import DFDesign.DB.Patch
    import compiler._
    val extendedPortsDB = designDB
      .patch(pullInZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.Replace)))
      .patch(pushOutZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.Replace)))
      .patch(scalaInZ.map((p, e) => p -> Patch.Add(e, Patch.Add.Config.Via)))
      .moveConnectableFirst
    val guardedMembers = topMembers.collect{case m : CanBeGuarded => m}
    val guardedDB = extendedPortsDB
      .patch(guardedMembers.head -> Patch.Add(control, Patch.Add.Config.Before))
      .patch(guardedMembers.map(m => m -> Patch.ChangeRef(m, (m : DFMember) => m.ownerRef, control.guard)))
    guardedDB
  }

  private val instName : String = designDB.top.name
  private val packName : String = designDB.top.name
  private val className : String = s"${designDB.top.typeName}Node"
  private val vhdlName : String = s"${designDB.top.typeName}Source"
  private val clkName : String = "clk"
  private val rstName : String = "rst"

  val pullInStr : String = pullInputs.map(p => s"""\n\t\taddInputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PULL, 1);""").mkString
  val pullOutStr : String = pullOutputs.map(p => s"""\n\t\taddOutputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PULL, 1);""").mkString
  val pushInStr : String = pushInputs.map(p => s"""\n\t\taddInputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PUSH, 2);""").mkString
  val pushOutStr : String = pushOutputs.map(p => s"""\n\t\taddOutputStream("${p.name}", ${p.width}, nodeClock, CustomNodeFlowControl.PUSH, 2);""").mkString
  val scalarInStr : String = scalarInputs.map(p => s"""\n\t\taddScalarInput("${p.name}", ${p.width});""").mkString
  val scalarOutStr : String = scalarOutputs.map(p => s"""\n\t\taddScalarOutput("${p.name}", ${p.width});""").mkString

  val nodeMaxJString : String =
    s"""
       |package $packName;
       |
       |import com.maxeler.maxcompiler.v2.managers.custom.CustomManager;
       |import com.maxeler.maxcompiler.v2.managers.custom.blocks.CustomHDLNode;
       |
       |final class $className extends CustomHDLNode {
       |	ScalarHDLNode(CustomManager manager, String instance_name) {
       |		super(manager, instance_name, "$instName");
       |
       |		CustomNodeClock nodeClock = addClockDomain("$clkName");
       |		nodeClock.setNeedsReset("$rstName");
       |		$pullInStr$pullOutStr$pushInStr$pushOutStr$scalarInStr$scalarOutStr
       |
       |		addVHDLSource("$vhdlName.vhdl", false);
       |	}
       |}
       |""".stripMargin
}
