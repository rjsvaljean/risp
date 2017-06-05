package rxvl.risp

import org.scalatest.words.ShouldVerb
import org.scalatest.{ FlatSpec, Matchers }

import model._

class ParserSpec
    extends FlatSpec
    with Matchers
    with ShouldVerb {
  behavior of "Parser"

  it should "1" in {
    Parser.parse("1").get.value should be(Expr.Number(1))
  }
}
