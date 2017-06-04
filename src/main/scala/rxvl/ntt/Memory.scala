package rxvl.ntt

import shapeless.nat



object Memory2 {
  import shapeless.nat._
  import Gates._
  import ArithematicGates.{toBin, fromBin}

  type IN_T0 = Boolean
  type IN_T1 = Boolean
  type OUT_T0 = Boolean

  case class SeqGate(run: IN_T0 => (IN_T1, OUT_T0)) {
    def andThen(f: Boolean => SeqGate): SeqGate = SeqGate { in_T0 =>
      val (in_T1, out_T0) = run(in_T0)
      f(out_T0).run(in_T1)
    }
  }

  object SeqGate {
    def sequence( gates: List[SeqGate], f: Boolean => Unit = _ => () ): SeqGate = {
      require(gates.nonEmpty)
      gates.reduce((g1, g2) => g1 andThen { out =>
        f(out)
        g2
      })
    }
  }

  def DFF(inT1: Boolean): SeqGate = SeqGate(inT0 => (inT1, inT0))

  def Bit(in: Boolean, load: Boolean): SeqGate = SeqGate { inT0 =>
    (inT0, Mux(load, inT0, in))
  } andThen { inT0 =>
    DFF(inT0)
  }

  type Byte16 = Word[_16, Boolean]
  type Address = Word[_8, Boolean]
  type RAMState = Map[Address, SeqGate]
  val Null: Word[_16, Boolean] = Word[_16](false)
  type IN_BYTE_T0 = Word[_16, Boolean]
  type IN_BYTE_T1 = Word[_16, Boolean]
  type OUT_BYTE_T0 = Word[_16, Boolean]


  case class SeqByteGate(run: IN_BYTE_T0 => (IN_BYTE_T1, OUT_BYTE_T0)) {
    def andThen(f: Byte16 => SeqByteGate): SeqByteGate = SeqByteGate { in_T0 =>
      val (in_T1, out_T0) = run(in_T0)
      f(out_T0).run(in_T1)
    }
  }

  object SeqByteGate {
    def sequence( gates: List[SeqByteGate], f: Byte16 => Unit = _ => () ): SeqByteGate = {
      require(gates.nonEmpty)
      gates.reduce((g1, g2) => g1 andThen { out =>
        f(out)
        g2
      })
    }

  }

  def DFF16(inT1: Byte16): SeqByteGate = SeqByteGate(inT0 => (inT1, inT0))

  def Bit16(in: Byte16, load: Boolean): SeqByteGate = SeqByteGate { inT0 =>
    (inT0, Mux16(Word[_16](load), inT0, in))
  } andThen { inT0 =>
    DFF16(inT0)
  }

  def testBit = SeqGate.sequence(List(
    (true, true),
    (false, false),
    (false, false),
    (false, true),
    (true, true),
    (false, false)
  ).map((Bit _).tupled), println).run(false)

  def testBit16 = SeqByteGate.sequence(List(
    (Word[_16](true), true),
    (Word[_16](false), false),
    (Word[_16](false), false),
    (Word[_16](false), true),
    (Word[_16](true), true),
    (Word[_16](false), false)
  ).map((Bit16 _).tupled), println).run(Word[_16](false))



  def RAM(
    load: Boolean,
    in: Byte16,
    address: Address
  ): Map[Address, SeqByteGate] => (Option[Byte16], Map[Address, SeqByteGate]) = { ram: Map[Address, SeqByteGate] =>
    if (load) (None, ram.updated(address, ram(address).andThen(_ => Bit16(in, load))))
    else (Some(ram(address).run(Word[_16](true))._2), ram)
  }


  val addr1 = Word[_8](true)
  val addr2 = Word[_8](false)
  val addr3 = Word[_8, Boolean](false +: Vector.fill(7)(true))
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

}
object Memory {


  import ArithematicGates._

  import shapeless.nat._
  import scalaz._
  import Scalaz._
  import scalaz.syntax.unzip.ToUnzipPairOps
  import Gates._

  def DFF(in: Boolean): State[Boolean, Boolean] = {
    State(b => (in, b))
  }

  def Bit(in: Boolean, load: Boolean): State[Boolean, Boolean] = {
    State.gets(Mux(load, _: Boolean, in)).flatMap(DFF)
  }

  type Byte16 = Word[_16, Boolean]
  type Address = Word[_8, Boolean]
  type Register = State[Byte16, Byte16]
  type RAMState = Map[Address, Register]
  val Null: Word[_16, Sum] = Word[_16](false)
  val NullRegister: Register = State.gets(identity[Byte16])

  def Bit16(in: Byte16, load: Boolean): Register =
    State { w: Byte16 =>
      val x = in.map(Bit(_, load))
      val y = x.fzip(w).map { case (_x, _w) => _x.run(_w) }
      y.unfzip
    }

  def RAM(load: Boolean,
          in: Byte16,
          address: Address
  ): State[RAMState, Byte16] = {
    val x = if (load)
      State.modify(
        (_: RAMState) + (address -> Bit16(in, load))
      ).map(_ => NullRegister)
    else
      State.gets((_: RAMState).getOrElse(address, NullRegister))
    x.map(_.run(Null)._1)
  }

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
  val addr3 = Word[_8, Boolean](false +: Vector.fill(7)(true))
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
