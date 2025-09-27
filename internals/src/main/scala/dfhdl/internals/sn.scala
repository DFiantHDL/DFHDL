package dfhdl.internals

extension (sc: StringContext)
  def sn(args: Any*): String =
    // create unique marker tokens for each arg (use NUL to avoid accidental collisions)
    val markers = args.indices.map(i => s"\u0000ARG$i\u0000").toArray

    // build a template that contains those markers in place of the interpolations
    val parts = sc.parts.toArray
    val tmplWithMarkers =
      val sb = StringBuilder()
      for i <- parts.indices do
        sb.append(parts(i))
        if i < markers.length then sb.append(markers(i))
      sb.toString

    // strip margin on both the marker-template and the actual interpolated text
    val tmplStripped = tmplWithMarkers.stripMargin
    val interpolatedStripped = sc.s(args*).stripMargin

    // split lines (preserve empty lines)
    val tLines = tmplStripped.betterLinesIterator.toVector
    val rLines = interpolatedStripped.betterLinesIterator.toVector

    // We'll reconstruct each template line replacing markers with the actual arg strings
    // and compare the expanded pieces to decide whether to drop them.
    var rIdx = 0
    val out = scala.collection.mutable.ArrayBuffer.empty[String]

    for tLine <- tLines do
      // expand markers in this single template line with the actual arg.toString
      var expanded = tLine
      for i <- args.indices do
        expanded =
          expanded.replace(markers(i), if args(i) equals null then "null" else args(i).toString)

      // splitting the expanded template-line may produce multiple result-lines if an arg contains newlines
      val expPieces = expanded.split("\n", -1).toSeq // keep trailing empty pieces
      val numPieces = expPieces.length

      // Safety: take the corresponding slice from the fully interpolated result (if available)
      val slice =
        if rIdx + numPieces <= rLines.length then rLines.slice(rIdx, rIdx + numPieces)
        else expPieces // fallback (should be rare)

      // Decide whether this template line was "only interpolations" (no non-whitespace literal)
      val containsMarker = markers.exists(m => tLine.contains(m))
      val tLineWithoutMarkers =
        markers.foldLeft(tLine)((s, m) => s.replace(m, ""))

      val onlyMarkersAndWhitespace = containsMarker && tLineWithoutMarkers.forall(_.isWhitespace)

      // Decide whether the expanded pieces are all empty (i.e. every interpolation produced "")
      val allExpPiecesEmpty = expPieces.forall(_.isEmpty)

      // Drop only if both true: template line had only markers/whitespace AND expansion produced only empty pieces
      if !(onlyMarkersAndWhitespace && allExpPiecesEmpty) then
        out ++= slice

      rIdx += numPieces
    end for

    // append any leftover result lines (robustness)
    while rIdx < rLines.length do
      out += rLines(rIdx); rIdx += 1

    out.mkString("\n")
end extension
