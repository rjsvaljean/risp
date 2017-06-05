package rxvl.ntt

import shapeless.nat

object Memory {
  import ArithematicGates._

  sealed trait ClockState
  case object Tick extends ClockState
  case object Tock extends ClockState

  import shapeless.nat._
  import scalaz.State
  import scalaz.Zip
  import scalaz.Functor
  import scalaz.Traverse
  import scalaz.Unzip
  import scalaz.syntax.functor.ToFunctorOps
  import scalaz.syntax.unzip.ToUnzipPairOps
  import scalaz.syntax.traverse.ToTraverseOps
  import scalaz.std.list.listInstance
  import scalaz.std.vector.vectorInstance
  import scalaz.syntax.zip.ToZipOps
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
  type Address = Word[_9, Boolean]
  type Register = State[Byte16, Byte16]
  val Null: Byte16 = Word[_16](false)
  val NullRegister: Register = State.gets(identity[Byte16])

  def runAll[F[_]: Zip: Functor: Unzip, S, A](
    sts: F[State[S, A]]
  ): State[F[S], F[A]] =
    State(
      _.fzip(sts)
       .map { case (in, st) => st.run(in) }
       .unfzip
    )

  def Register(in: Byte16, load: Boolean): Register = {
    runAll(in.map(Bit(_, load)))
  }

  def RAM8(load: Boolean,
           in: Byte16,
           address: Word[_3, Boolean]
  ): State[Vector[Byte16], Byte16] = State { s =>
    val (l0, l1, l2, l3, l4, l5, l6, l7) = DMux8Way(address, load)

    val (ns0, o0) = Register(in, l0).run(s(0))
    val (ns1, o1) = Register(in, l1).run(s(1))
    val (ns2, o2) = Register(in, l2).run(s(2))
    val (ns3, o3) = Register(in, l3).run(s(3))
    val (ns4, o4) = Register(in, l4).run(s(4))
    val (ns5, o5) = Register(in, l5).run(s(5))
    val (ns6, o6) = Register(in, l6).run(s(6))
    val (ns7, o7) = Register(in, l7).run(s(7))

    (
      Vector(ns0, ns1, ns2, ns3, ns4, ns5, ns6, ns7),
      Mux8Way16( address, o0, o1, o2, o3, o4, o5, o6, o7 )
    )
  }

  def RAM64(load: Boolean,
            in: Byte16,
            address: Word[_6, Boolean]
  ): State[Vector[Vector[Byte16]], Byte16] = State { s =>
    val selFirst = Word[ _3, Carry ]( address.bits.take( 3 ) )
    val selSecond = Word[ _3, Carry ]( address.bits.takeRight( 3 ) )
    val (l0, l1, l2, l3, l4, l5, l6, l7) = DMux8Way(selSecond, load)
    val (ns0, o0) = RAM8(l0, in, selFirst).run(s(0))
    val (ns1, o1) = RAM8(l1, in, selFirst).run(s(1))
    val (ns2, o2) = RAM8(l2, in, selFirst).run(s(2))
    val (ns3, o3) = RAM8(l3, in, selFirst).run(s(3))
    val (ns4, o4) = RAM8(l4, in, selFirst).run(s(4))
    val (ns5, o5) = RAM8(l5, in, selFirst).run(s(5))
    val (ns6, o6) = RAM8(l6, in, selFirst).run(s(6))
    val (ns7, o7) = RAM8(l7, in, selFirst).run(s(7))
    (
      Vector( ns0, ns1, ns2, ns3, ns4, ns5, ns6, ns7 ),
      Mux8Way16( selSecond, o0, o1, o2, o3, o4, o5, o6, o7 )
    )
  }

  def RAM512(load: Boolean,
             in: Byte16,
             address: Word[_9, Boolean]
  ): State[Vector[Vector[Vector[Byte16]]], Byte16] = State { s =>
    val selFirst = Word[ _6, Carry ]( address.bits.take( 6 ) )
    val selSecond = Word[ _3, Carry ]( address.bits.takeRight( 3 ) )
    val (l0, l1, l2, l3, l4, l5, l6, l7) = DMux8Way(selSecond, load)
    val (ns0, o0) = RAM64(l0, in, selFirst).run(s(0))
    val (ns1, o1) = RAM64(l1, in, selFirst).run(s(1))
    val (ns2, o2) = RAM64(l2, in, selFirst).run(s(2))
    val (ns3, o3) = RAM64(l3, in, selFirst).run(s(3))
    val (ns4, o4) = RAM64(l4, in, selFirst).run(s(4))
    val (ns5, o5) = RAM64(l5, in, selFirst).run(s(5))
    val (ns6, o6) = RAM64(l6, in, selFirst).run(s(6))
    val (ns7, o7) = RAM64(l7, in, selFirst).run(s(7))
    (
      Vector( ns0, ns1, ns2, ns3, ns4, ns5, ns6, ns7 ),
      Mux8Way16( selSecond, o0, o1, o2, o3, o4, o5, o6, o7 )
    )
  }

}
