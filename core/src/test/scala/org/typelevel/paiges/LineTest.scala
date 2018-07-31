package org.typelevel.paiges

import org.scalatest.FunSuite
import org.typelevel.paiges.Doc._

class LineTest extends FunSuite {

  // same as `line` except `lineNoFlat.flatBoolean` returns (this, changed=false).
  // Required for line breaks that are treated as semicolons: { a\nb }
  val lineNoFlat = hardLine

  // same as `lineNoFlat` except it does not respect nesting.
  // Required to print verbatim line breaks such as in multiline strings: """\n""".
  val lineNoFlatNoNesting = hardLine

  test("lineNoFlat cancels grouping") {
    assert("\n\n" == (line + lineNoFlat).grouped.render(100))
  }

  test("lineNoFlat preserves nesting") {
    assert("\n  \n  " == (line + lineNoFlat).grouped.nested(2).render(100))
  }

  // We're ignoring the following tests until we add a combinator that
  // supports un-indented line breaks.

  ignore("lineNoFlatNoIndent cancels grouping") {
    assert("\n\n" == (line + lineNoFlatNoNesting).grouped.render(100))

  }

  ignore("lineNoFlatNoIndent ignores nesting") {
    assert(
      "\n  \n" == (line + lineNoFlatNoNesting).grouped.nested(2).render(100))
  }

  ignore("it's possible to use print multiline strings and use .grouped") {
    val tripleQuote = text("'''")
    val multilineString = tripleQuote + lineNoFlatNoNesting + tripleQuote
    val doc = text("foo") + multilineString.tightBracketBy(text("("), text(")"))
    assert(
      doc.grouped.render(100) ==
        """foo(
          |  '''
          |'''
          |)""".stripMargin
    )
  }

}