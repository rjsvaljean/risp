package rxvl.ntt

import shapeless.nat

object Memory {
  import ArithematicGates._

  sealed trait ClockState
  case object Tick extends ClockState
  case object Tock extends ClockState

  import shapeless.nat._
  import scalaz._
  import Scalaz._
  import scalaz.syntax.unzip.ToUnzipPairOps
  import Gates._

  val clock = Stream.iterate[ClockState](Tick) {
    case Tock => Tick
    case Tick => Tock
  }

  def DFF(in: Boolean): State[Boolean, Boolean] = {
    State(b => (in, b))
  }

  def Bit(in: Boolean, load: Boolean): State[Boolean, Boolean] = {
    State.gets(Mux(load, _: Boolean, in)).flatMap(DFF)
  }

  type Byte16 = Word[_16, Boolean]
  type Address = Word[_8, Boolean]
  type RAMState = Map[Address, Byte16]
  val Null = Word[_16](false)

  def Bit16(in: Byte16, load: Boolean): State[Byte16, Byte16] =
    State { w: Byte16 =>
      val x = in.map(Bit(_, load))
      val y = x.fzip(w).map { case (_x, _w) => _x.run(_w) }
      y.unfzip
    }

  def RAM(load: Boolean, in: Byte16, address: Address): State[RAMState, Byte16] =
    if (load)
      State.modify((_: RAMState) + (address -> in)).map(_ => Null)
    else
      State.gets((_: RAMState).getOrElse(address, Null))

  def testBit = List(
    (true, true),
    (false, false),
    (false, false),
    (false, true),
    (true, true),
    (false, false)
  ).map((Bit _).tupled).sequence.run(false)

  val addr1 = Word[_8](true)
  val addr2 = Word[_8](false)
  val sixteen = toBin(16)
  val thirtyTwo = toBin(32)
  val fifteen = toBin(15)
  def testRam = List(
    (true, sixteen, addr1),
    (true, thirtyTwo, addr2),
    (false, Word[_16](true), addr2),
    (false, Word[_16](true), addr1),
    (true, fifteen, addr1),
    (false, Word[_16](true), addr1)
  ).map((RAM _).tupled).sequence.run(Map())._2.map(fromBin)

  def testBit16 = List(
    (Word[_16](true), true),
    (Word[_16](false), false),
    (Word[_16](false), false),
    (Word[_16](false), true),
    (Word[_16](true), true),
    (Word[_16](false), false)
  ).map((Bit16 _).tupled).sequence.run(Word[_16](false))

  def testDFF = List(
    true,
    false,
    false,
    true,
    false,
    false
  ).map(DFF).sequence.run(true)
}
