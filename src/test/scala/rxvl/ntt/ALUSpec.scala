package rxvl.ntt

import org.scalatest._
import org.scalacheck._
import org.scalatest.prop.PropertyChecks
import rxvl.ntt.ALU._

class ALUSpec
  extends FlatSpec
    with Matchers
    with PropertyChecks {
  import ALU._

  behavior of "ALU"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(workers = 4, minSuccessful = 5000, maxDiscarded = 0)

  implicit val ar: Arbitrary[Op] = Arbitrary(Gen.oneOf(Seq(
    Zero, One, MinusOne, JustX, JustY, JustNotX,
    JustNotY, JustNegX, JustNegY, IncX, IncY,
    DecX, DecY, Plus, XMinusY, YMinusX, AndOp,
    OrOp
  )))

  implicit val signed16BitInts: Arbitrary[Int] = {
    val max = Math.pow(2, 14).toInt
    Arbitrary(Gen.choose(max * -1, max))
  }

  it should "compute all the things" in {
    forAll { (x: Int, y: Int, op: Op) =>
      ALU.testD(op)(x, y) should be(getF(op)(x, y))
    }
  }

  def getF(op: Op): (Int, Int) => Int = {
    op match {
      case Zero => (_, _) => 0
      case One => (_, _) => 1
      case MinusOne => (_, _) => -1
      case JustX => (x, _) => x
      case JustY => (_, y) => y
      case JustNotX => (x, _) => ~x
      case JustNotY => (_, y) => ~y
      case JustNegX => (x, _) => -x
      case JustNegY => (_, y) => -y
      case IncX => (x, _) => x + 1
      case IncY => (_, y) => y + 1
      case DecX => (x, _) => x - 1
      case DecY => (_, y) => y - 1
      case Plus => (x, y) => x + y
      case XMinusY => (x, y) => x - y
      case YMinusX => (x, y) => y - x
      case AndOp => (x, y) => x & y
      case OrOp => (x, y) => x | y
    }
  }
}
