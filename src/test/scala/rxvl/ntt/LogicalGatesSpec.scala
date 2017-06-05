package rxvl.ntt

import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.function._
import shapeless.ops.nat._
import shapeless.ops.product._
import org.scalatest._
import LogicalGates._
import GateHelpers._

class LogicalGatesSpec extends FlatSpec with Matchers {
  implicit object toHListB extends ToHList[Boolean] {
    type Out = Boolean :: HNil
    def apply(t: Boolean): Boolean :: HNil = t :: HNil
  }

  it should "test Not" in {
    truthTable(Not _) should be(Set(
      List(true) -> List(false),
      List(false) -> List(true)
    ))
  }

  it should "test NAnd" in {
    truthTable(NAnd _) should be(Set(
      List(true, true) -> List(false),
      List(true, false) -> List(true),
      List(false, true) -> List(true),
      List(false, false) -> List(true)
    ))
  }

  it should "test And" in {
    truthTable(And _) should be(Set(
      List(true, true) -> List(true),
      List(true, false) -> List(false),
      List(false, true) -> List(false),
      List(false, false) -> List(false)
    ))
  }

  it should "test Or" in {
    truthTable(Or _) should be(Set(
      List(true, true) -> List(true),
      List(true, false) -> List(true),
      List(false, true) -> List(true),
      List(false, false) -> List(false)
    ))
  }

  it should "test NOr" in {
    truthTable(NOr _) should be(Set(
      List(true, true) -> List(false),
      List(true, false) -> List(false),
      List(false, true) -> List(false),
      List(false, false) -> List(true)
    ))
  }

  it should "test XOr" in {
    truthTable(XOr _) should be(Set(
      List(true, true) -> List(false),
      List(true, false) -> List(true),
      List(false, true) -> List(true),
      List(false, false) -> List(false)
    ))
  }

  it should "test Mux" in {
    truthTable(Mux _) should be(Set(
      List(true, true, true) -> List(true),
      List(true, true, false) -> List(false),
      List(true, false, true) -> List(true),
      List(false, true, true) -> List(true),
      List(false, false, true) -> List(false),
      List(false, true, false) -> List(true),
      List(true, false, false) -> List(false),
      List(false, false, false) -> List(false)
    ))
  }

  it should "test DMux" in {
    truthTable(DMux _) should be(Set(
      List(true, true) -> List(false, true),
      List(true, false) -> List(false, false),
      List(false, true) -> List(true, false),
      List(false, false) -> List(false, false)
    ))
  }
}

object GateHelpers {

  def truthTable[In <: HList, N <: Nat, Fn, Out, OutL <: HList](f: Fn)(
    implicit
    ev1: FnToProduct.Aux[Fn, In => Out],
    ev2: ToList[In, Boolean],
    ev3: Length.Aux[In, N],
    ev4: ToInt[N],
    ev5: ToHList.Aux[Out, OutL],
    ev6: ToList[OutL, Boolean]
  ): Set[(List[Boolean], List[Boolean])] = {
    gateInputs(ev4())
      .map(_.foldRight(HNil: HList)(_ :: _).asInstanceOf[In])
      .map(ins => ev2(ins) -> ev5(ev1(f)(ins)).toList)
      .toSet
  }

  def gateInputs(n: Int): List[List[Boolean]] = {
    val ins = List(true, false)
    List.fill(n - 1)(ins)
      .foldLeft(ins.map(List(_))) { (acc: List[List[Boolean]], in: List[Boolean]) =>
        in.flatMap(i => acc.map(i :: _))
      }
  }

}
