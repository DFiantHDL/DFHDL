package dfhdl.compiler
package printing
import ir.*
import dfhdl.internals.*
import scala.collection.mutable

trait AbstractTypePrinter extends AbstractPrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String
  def csDFBits(dfType: DFBitsWL, typeCS: Boolean): String
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String
  final def csNamedDFTypeDcl(dfType: NamedDFType, global: Boolean): String =
    val dcl = dfType match
      case dt: DFEnum   => csDFEnumDcl(dt, global)
      case dt: DFOpaque => csDFOpaqueDcl(dt)
      case dt: DFStruct => csDFStructDcl(dt)
    val doc = printer.csDocString(dfType.meta)
    if (doc.isEmpty) dcl else s"$doc\n$dcl"
  private def isInt32Val(member: DFMember): Boolean =
    member match
      case dfVal: DFVal =>
        dfVal.dfType match
          case DFInt32 => true
          case _       => false
      case _ => false
  // Per-sub-printer fresh global members for a hierarchical root: each sub-DB's
  // global members not yet emitted by an earlier sub-DB (first-occurrence dedup
  // in sub-DB iteration order), printed under that sub-DB's getSet. A flat DB is
  // a single (this, all globals) group.
  private def globalConstGroups: List[(TPrinter, List[DFMember])] =
    val designDB = getSet.designDB
    if (designDB.isRoot)
      val seen = mutable.HashSet.empty[DFMember]
      designDB.subDBs.view.values.flatMap { sub =>
        val fresh = sub.membersGlobals.filter(seen.add)
        Option.when(fresh.nonEmpty)(withGetSet(sub.getSet) -> fresh)
      }.toList
    else List(printer -> designDB.membersGlobals)
  // the global constants flattened out of their groups, each paired with the printer that
  // renders it (and whose getSet resolves its references)
  protected final def globalConstsWithPrinters: List[(TPrinter, DFMember)] =
    globalConstGroups.flatMap((p, gs) => gs.map(p -> _))
  final def csGlobalConstIntDcls: String =
    globalConstGroups.iterator
      .map((p, gs) => p.csDFMembers(gs.filter(isInt32Val)))
      .filter(_.nonEmpty).mkString("\n")
  final def csGlobalConstNonIntDcls: String =
    globalConstGroups.iterator
      .map((p, gs) => p.csDFMembers(gs.filterNot(isInt32Val)))
      .filter(_.nonEmpty).mkString("\n")
  // Every global named type across sub-DBs (first-occurrence dedup in sub-DB
  // iteration order), each paired with the printer that renders it.
  private def globalTypeGroups: List[(TPrinter, List[NamedDFType])] =
    val designDB = getSet.designDB
    if (designDB.isRoot)
      val seen = mutable.HashSet.empty[NamedDFType]
      designDB.subDBs.view.values.flatMap { sub =>
        val fresh = sub.getGlobalNamedDFTypes.iterator.filter(seen.add).toList
        Option.when(fresh.nonEmpty)(withGetSet(sub.getSet) -> fresh)
      }.toList
    else List(printer -> designDB.getGlobalNamedDFTypes.toList)
  private def typeDclFilter(dfType: NamedDFType): Boolean = dfType match
    // show tuple structures only if tuple support is disabled
    case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
    // skipping unknown clock and reset definitions (they are unknown because
    // they lack additional name suffix that belongs to their configuration)
    case t: DFOpaque if t.name == "Clk" && t.kind == DFOpaque.Kind.Clk => false
    case t: DFOpaque if t.name == "Rst" && t.kind == DFOpaque.Kind.Rst => false
    case _                                                             => true
  // The dedicated-package type declarations: (package name, namespace, content) in
  // cross-package dependency order (a struct field may be a type of another package),
  // first-appearance order among independent packages. Each declaration renders under
  // its package context so its own (and same-package) type names print unqualified.
  // Two distinct namespaces mapping to one package name (the documented `top.x` vs
  // root-level `x` clash) are rejected.
  final def packagedTypeEntries: List[(String, String, List[(TPrinter, NamedDFType)])] =
    val designDB = getSet.designDB
    // design-local named types with a foreign namespace are packaged too (placement
    // overrides design-locality); a type used locally by several designs dedups
    val localGroups: List[(TPrinter, List[NamedDFType])] =
      if (designDB.isRoot)
        designDB.subDBs.view.values.flatMap { sub =>
          val locals = sub.getLocalNamedDFTypes(sub.top).toList
          Option.when(locals.nonEmpty)(withGetSet(sub.getSet) -> locals)
        }.toList
      else
        designDB.designMemberList.view.map(_._1).flatMap { design =>
          val locals = designDB.getLocalNamedDFTypes(design).toList
          Option.when(locals.nonEmpty)(printer -> locals)
        }.toList
    val perPkg =
      mutable.LinkedHashMap.empty[String, (String, mutable.ListBuffer[(TPrinter, NamedDFType)])]
    val seen = mutable.HashSet.empty[NamedDFType]
    (globalTypeGroups ++ localGroups).foreach { (p, types) =>
      types.view.filter(typeDclFilter).filter(seen.add).foreach { t =>
        p.typePlacementOf(t).foreach { pkg =>
          val (ns, buf) = perPkg.getOrElseUpdate(pkg, (t.meta.namespace, mutable.ListBuffer.empty))
          if (ns != t.meta.namespace)
            throw new IllegalArgumentException(
              s"Namespaces `$ns` and `${t.meta.namespace}` both map to the emitted package `$pkg`."
            )
          buf += ((p, t))
        }
      }
    }
    // topological order across packages by named-type references
    def depsOf(pkg: String): List[String] =
      perPkg(pkg)._2.view.flatMap { (p, t) =>
        given MemberGetSet = p.getSet
        t.decompose { case n: NamedDFType => n }.view
          .filterNot(_ == t)
          .flatMap(p.typePlacementOf)
          .filter(_ != pkg)
      }.toList.distinct
    val ordered = mutable.ListBuffer.empty[String]
    val done = mutable.Set.empty[String]
    def place(pkg: String): Unit =
      if (done.add(pkg))
        depsOf(pkg).foreach(place)
        ordered += pkg
    perPkg.keys.foreach(place)
    ordered.view.map { pkg =>
      val (ns, entries) = perPkg(pkg)
      (pkg, ns, entries.toList)
    }.toList
  end packagedTypeEntries
  final def packagedTypeDcls: List[(String, String, String)] =
    packagedTypeEntries.map { (pkg, ns, entries) =>
      val dcls = entries.map { (p, t) =>
        p.currentPackage = Some(pkg)
        try p.csNamedDFTypeDcl(t, global = true)
        finally p.currentPackage = None
      }
      (pkg, ns, dcls.mkString("\n"))
    }
  // Design-local named types that packaged content references (e.g. a global-placed
  // struct that is a FIELD of a packaged struct): a package file cannot reference a
  // type declared inside a design, so these are hoisted into the general global file.
  final def packagedHoistedTypes: List[(TPrinter, NamedDFType)] =
    val packagedTypes = packagedTypeEntries.flatMap(_._3)
    val globalTypes = globalTypeGroups.flatMap(_._2).toSet
    val seen = mutable.HashSet.empty[NamedDFType]
    packagedTypes.flatMap { (p, t) =>
      given MemberGetSet = p.getSet
      t.decompose { case n: NamedDFType => n }.view
        .filterNot(_ == t)
        .filter(n => p.typePlacementOf(n).isEmpty)
        .filterNot(globalTypes)
        .filter(seen.add)
        .map(p -> _)
        .toList
    }
  final def csGlobalTypeDcls: String =
    val globalPlaced = globalTypeGroups.iterator.flatMap { (p, types) =>
      types.view
        .filter(typeDclFilter)
        .filter(t => p.typePlacementOf(t).isEmpty)
        .map(x => p.csNamedDFTypeDcl(x, global = true))
    }
    val hoisted = packagedHoistedTypes.view
      .filter((_, t) => typeDclFilter(t))
      .map((p, t) => p.csNamedDFTypeDcl(t, global = true))
    (globalPlaced ++ hoisted).mkString("\n")
  end csGlobalTypeDcls
  final def csLocalTypeDcls(design: DFDesignBlock): String =
    val designDB = getSet.designDB
    // Exclude types promoted to global across the hierarchy: a sub-DB sees only
    // its one design and may mis-classify a cross-design global type as local
    // (empty for a flat DB, which classifies named types directly).
    val hierGlobal = designDB.rootDB.hierGlobalNamedDFTypes
    val hoisted = packagedHoistedTypes.view.map(_._2).toSet
    designDB.getLocalNamedDFTypes(design).view
      .filterNot(hierGlobal)
      // a foreign-namespace type is never design-local: it is emitted into its package
      .filter(t => printer.typePlacementOf(t).isEmpty)
      // a type referenced by packaged content is hoisted to the global file
      .filterNot(hoisted)
      .filter {
        // show tuple structures only if tuple support is disabled
        case dfType: DFStruct if dfType.isTuple && tupleSupportEnable => false
        // skipping unknown clock and reset definitions (they are unknown because
        // they lack additional name suffix that belongs to their configuration)
        case t: DFOpaque if t.name == "Clk" && t.kind == DFOpaque.Kind.Clk => false
        case t: DFOpaque if t.name == "Rst" && t.kind == DFOpaque.Kind.Rst => false
        case _                                                             => true
      }
      .map(x => printer.csNamedDFTypeDcl(x, global = false))
      .mkString("\n")
  end csLocalTypeDcls
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String
  def csDFVector(dfType: DFVector, typeCS: Boolean): String
  def csDFOpaqueDcl(dfType: DFOpaque): String
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String
  def csDFStructDcl(dfType: DFStruct): String
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String
  def csDFDouble(): String
  def csDFTime(dfType: DFTime, typeCS: Boolean): String
  def csDFFreq(dfType: DFFreq, typeCS: Boolean): String
  def csDFNumber(dfType: DFNumber, typeCS: Boolean): String
  def csDFString(dfType: DFString, typeCS: Boolean): String

  final def csDFType(dfType: DFType, typeCS: Boolean = false): String = dfType match
    case dt: DFBoolOrBit                                  => csDFBoolOrBit(dt, typeCS)
    case dt: DFBitsWL                                     => csDFBits(dt, typeCS)
    case dt: DFDecimal                                    => csDFDecimal(dt, typeCS)
    case dt: DFEnum                                       => csDFEnum(dt, typeCS)
    case dt: DFVector                                     => csDFVector(dt, typeCS)
    case dt: DFOpaque                                     => csDFOpaque(dt, typeCS)
    case dt: DFStruct if dt.isTuple && tupleSupportEnable =>
      csDFTuple(dt.fieldMap.values.toList, typeCS)
    case dt: DFStruct  => csDFStruct(dt, typeCS)
    case dt: DFUnit    => csDFUnit(dt, typeCS)
    case DFDouble      => csDFDouble()
    case dt: DFTime    => csDFTime(dt, typeCS)
    case dt: DFFreq    => csDFFreq(dt, typeCS)
    case dt: DFNumber  => csDFNumber(dt, typeCS)
    case dt: DFString  => csDFString(dt, typeCS)
    case dt: DFNothing => ???
end AbstractTypePrinter

protected trait DFTypePrinter extends AbstractTypePrinter:
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "Boolean"
    case DFBit  => "Bit"
  def csDFBits(dfType: DFBitsWL, typeCS: Boolean): String =
    if (dfType.lowIdxRef.equals(0))
      val csWidth = dfType.widthParamRef.refCodeString(typeCS)
      if (typeCS) s"Bits[$csWidth]"
      else s"Bits($csWidth)"
    else
      val csHigh = dfType.widthParamRef.hboundCS(dfType.lowIdxRef, typeCS)
      val csLow = dfType.lowIdxRef.refCodeString(typeCS)
      if (typeCS) s"BitsHL[$csHigh, $csLow]"
      else s"BitsHL($csHigh, $csLow)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    // the magnitude-width code string is the total width for integer types (fractionWidth
    // == 0) and the integer-part width for fixed-point types
    val csMagWidth = dfType.magnitudeWidthParamRef.refCodeString(typeCS)
    val (ob, cb) = if (typeCS) ("[", "]") else ("(", ")")
    (signed, fractionWidth) match
      case (false, 0) => s"UInt$ob$csMagWidth$cb"
      case (true, 0)  =>
        if (dfType.isDFInt32) "Int"
        else s"SInt$ob$csMagWidth$cb"
      case (false, _) => s"UFix$ob$csMagWidth, $fractionWidth$cb"
      case (true, _)  => s"SFix$ob$csMagWidth, $fractionWidth$cb"
  def csDFString(dfType: DFString, typeCS: Boolean): String = "String"
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.name
    val width = dfType.widthIntOpt.get
    val entries =
      dfType.entries.view
        .map((n, v) =>
          s"case $n extends $enumName(${printer.csDFDecimalData(DFUInt(IntParamRef(width)), Some(v))})"
        )
        .mkString("\n")
        .hindent
    s"enum ${enumName}(val value: ${csDFDecimal(DFUInt(IntParamRef(width)), true)} <> CONST) extends Encoded.Manual($width):\n$entries"
  // full-namespace qualification of a packaged type's reference (`<namespace>.<name>`),
  // dropped inside its own package section; qualification, not imports, so same-named
  // types from different packages can never collide
  protected def nsQualifier(dfType: NamedDFType): String =
    printer.typePlacementOf(dfType) match
      case Some(pkg) if !printer.currentPackage.contains(pkg) => s"${dfType.meta.namespace}."
      case _                                                  => ""
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = s"${nsQualifier(dfType)}${dfType.name}"
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val dimStr =
      if (cellDimParamRefs.size == 1) cellDimParamRefs.head.refCodeString(typeCS).applyBrackets()
      else cellDimParamRefs.map(_.refCodeString(typeCS)).mkStringBrackets
    s"${csDFType(cellType, typeCS)} X $dimStr"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    val csActualType = csDFType(dfType.actualType)
    val extendee = dfType.kind match
      case DFOpaque.Kind.Clk    => s"Clk"
      case DFOpaque.Kind.Rst    => s"Rst"
      case DFOpaque.Kind.Magnet => s"Magnet($csActualType)"
      case _                    => s"Opaque($csActualType)"
    s"case class ${dfType.name}() extends $extendee"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String =
    s"${nsQualifier(dfType)}${dfType.name}"
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n}${csDFValType(t)}")
      .mkString("\n")
      .hindent(2)
    s"final case class ${dfType.name}(\n$fields\n) extends Struct"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String =
    s"${nsQualifier(dfType)}${dfType.name}"
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = "Unit"
  def csDFDouble(): String = "Double"
  def csDFTime(dfType: DFTime, typeCS: Boolean): String = "Time"
  def csDFFreq(dfType: DFFreq, typeCS: Boolean): String = "Freq"
  def csDFNumber(dfType: DFNumber, typeCS: Boolean): String = "Number"
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String =
    fieldList.view.map(f => csDFType(f, typeCS)).mkStringBrackets
  def csDFValType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> VAL"
  def csDFValConstType(dfType: DFType): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> CONST"
  // a procedure argument's directional type (`<> IN` / `<> OUT`)
  def csDFValPortType(dfType: DFType, dirCS: String): String =
    s": ${printer.csDFType(dfType, typeCS = true)} <> $dirCS"
end DFTypePrinter
