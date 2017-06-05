package rxvl.ntt

import org.scalatest.{FlatSpec, Matchers}
import shapeless.nat.{_3, _6, _9, _12}

class MemorySpec extends FlatSpec with Matchers {
  behavior of "Memory"
  import Memory._
  import ArithematicGates.toBin
  import ArithematicGates.fromBin
  import scalaz.std.vector.vectorInstance
  import scalaz.syntax.traverse.ToTraverseOps

  private val fourteen = toBin(14)
  private val fifteen = toBin(15)
  private val sixteen = toBin(16)

  private val addrs8 =
    GateHelpers.gateInputs(3).toVector.map(bs => Word[_3, Boolean](bs.toVector))

  private val addrs16 =
    GateHelpers.gateInputs(6).toVector.map(bs => Word[_6, Boolean](bs.toVector))

  private val addrs512 =
    GateHelpers.gateInputs(9).toVector.map(bs => Word[_9, Boolean](bs.toVector))

  it should "save some stuff to RAM8" in {
    addrs8.foreach { addr8 =>
      val (state: Vector[Byte16], outs: Vector[Byte16]) = Vector(
        RAM8(load = true, fourteen, addr8),
        RAM8(load = false, Null, addr8),
        RAM8(load = true, fifteen, addr8),
        RAM8(load = false, Null, addr8)
      ).sequence.run(Vector.fill(8)(Null))
      state(fromBin(addr8.bits)) should be(fifteen)
      state.patch(fromBin(addr8.bits), Nil, 1) should contain only(Null)
      outs should be(Vector(
        Null,
        fourteen,
        fourteen,
        fifteen
      ))
    }
  }

  it should "save some stuff to RAM64" in {
    addrs16.foreach { addr16 =>
      val (state: Vector[Vector[Byte16]], outs: Vector[Byte16]) = Vector(
        RAM64(load = true, fourteen, addr16),
        RAM64(load = false, Null, addr16),
        RAM64(load = true, fifteen, addr16),
        RAM64(load = false, Null, addr16)
      ).sequence.run(Vector.fill(8)(Vector.fill(8)(Null)))

      def memAssertion(ix: Int, iy: Int, n: Byte16) = {
        if (
          ix == fromBin(addr16.bits.drop(3)) &&
          iy == fromBin(addr16.bits.take(3))
        ) state(ix)(iy) should be(n)
        else state(ix)(iy) should be(Null)
      }

      for {x <- 0 to 7; y <- 0 to 7} { memAssertion(x, y, fifteen) }
      outs should be(Vector(
        Null,
        fourteen,
        fourteen,
        fifteen
      ))
    }
  }

  it should "save some stuff to RAM512" in {
    addrs512.foreach { addr512 =>
      val (state: Vector[Vector[Vector[Byte16]]], outs: Vector[Byte16]) = Vector(
        RAM512(load = true, fourteen, addr512),
        RAM512(load = false, Null, addr512),
        RAM512(load = true, fifteen, addr512),
        RAM512(load = false, Null, addr512)
      ).sequence.run(Vector.fill(8)(Vector.fill(8)(Vector.fill(8)(Null))))

      def memAssertion(ix: Int, iy: Int, iz: Int, n: Byte16) = {
        if (
          ix == fromBin(addr512.bits.drop(6)) &&
          iy == fromBin(addr512.bits.drop(3).take(3)) &&
          iz == fromBin(addr512.bits.take(3))
        ) state(ix)(iy)(iz) should be(n)
        else state(ix)(iy)(iz) should be(Null)
      }

      for {x <- 0 to 7; y <- 0 to 7; z <- 0 to 7} { memAssertion(x, y, z, fifteen) }
      outs should be(Vector(
        Null,
        fourteen,
        fourteen,
        fifteen
      ))
    }
  }

}
