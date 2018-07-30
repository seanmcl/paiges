package org.typelevel.paiges

import scala.annotation.tailrec
import org.scalatest.FunSuite
import org.typelevel.paiges.Doc._
import org.typelevel.paiges.Document.ops._

class LineTest extends FunSuite {

  test("lineNoFlat cancels grouping") {
    assert("\n\n" == (line + hardLine).grouped.render(100))
  }

  test("lineNoFlat preserves nesting") {
    assert("\n  \n  " == (line + hardLine).grouped.nested(2).render(100))
  }

  // Split a string on \n.  String.split doesn't work because it drops empty strings.
  def split(s: String): List[String] = {
    @tailrec def loop(pos: Int, cur: List[Char], acc: List[String]): List[String] =
      if (pos >= s.length) (cur.reverse.mkString :: acc).reverse
      else s.charAt(pos) match {
        case '\n' => loop(pos + 1, Nil, cur.reverse.mkString :: acc)
        case c => loop(pos + 1, c :: cur, acc)
      }
    loop(0, Nil, Nil)
  }
  def tripleQuote(s: String): Doc = {
    val quote3 = "'''".doc
    (quote3 + Doc.intercalate(Doc.hardLine, split(s).map(_.doc)).ignoreNesting + quote3).ignoreNesting
  }

  test("it's possible to use print multiline strings and use .grouped") {
    val doc = "foo".doc + "(".doc + tripleQuote("\n") + ")".doc
    assert(
      doc.grouped.render(100) ==
        """foo('''
          |''')""".stripMargin
    )
  }

  test("Multiline string 2") {
    val text = """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                 |  In at molestie quam, eget pretium tellus. Vestibulum elementum lobortis turpis,
                 |    volutpat eleifend risus lacinia ut.
                 |      Aenean in nulla velit.
                 |        Donec aliquam lacinia mattis.
                 |""".stripMargin
    val doc = ("foo".doc + "(".doc + tripleQuote(text) + ")".doc).grouped.nested(100)
    assert(
      doc.grouped.render(10000) ==
        s"""foo('''${text}''')""".stripMargin
    )
    assert(
      doc.grouped.nested(10).render(10000) ==
        s"""foo('''${text}''')""".stripMargin
    )
  }
}