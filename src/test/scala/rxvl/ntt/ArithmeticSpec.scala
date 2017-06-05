package rxvl.ntt

import org.scalatest._
import rxvl.ntt.ArithematicGates.{ FullAdder, HalfAdder }
import GateHelpers.truthTable

class ArithmeticSpec extends FlatSpec with Matchers {

  behavior of "ArithmeticGates"

  it should "test HalfAdder" in {
    truthTable(HalfAdder _) should be(Set(
      List(true, true) -> List(true, false),
      List(true, false) -> List(false, true),
      List(false, true) -> List(false, true),
      List(false, false) -> List(false, false)
    ))
  }

  it should "test FullAdder" in {
    truthTable(FullAdder _) should be(Set(
      List(true, true, true) -> List(true, true),
      List(true, false, true) -> List(true, false),
      List(false, true, true) -> List(true, false),
      List(false, false, true) -> List(false, true),
      List(true, true, false) -> List(true, false),
      List(true, false, false) -> List(false, true),
      List(false, true, false) -> List(false, true),
      List(false, false, false) -> List(false, false)
    ))
  }
}
