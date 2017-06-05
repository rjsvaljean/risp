package rxvl.ntt

object CPU {
  import Memory._
  import ArithematicGates.{ toBin, fromBin }
  import scalaz._
  import Scalaz._

  sealed trait Op
  case class Put(addr: Address, value: Byte16) extends Op
  case class Get(addr: Address) extends Op
  case class Calculate[A](
    op: ALU.Op,
    x: Address,
    y: Address,
    out: Address
  ) extends Op

  type CPUState[A] = State[Vector[Vector[Vector[Byte16]]], A]

  val run: Op => CPUState[Unit] = {
    case Put(addr, value) => RAM512(load = true, value, addr).map(_ => ())
    case Get(addr) =>
      RAM512(load = false, Null, addr).map(i => println(fromBin(i)))
    case Calculate(op, x, y, addr) =>
      Applicative[CPUState].apply2(
        RAM512(load = false, Null, x),
        RAM512(load = false, Null, y)
      )(ALU.test(op, _, _)).flatMap(o =>
          RAM512(load = true, o, addr).map(_ => ()))

  }

  def runScript(script: Seq[Op]): Unit =
    script.map(run).reduce((s1, s2) => s1.flatMap(_ => s2)).run(RAM512Init)._2

  //  def test = runScript(Seq(
  //    Put(addr1, thirtyTwo),
  //    Put(addr2, fifteen),
  //    Calculate(ALU.Plus, addr1, addr2, addr3),
  //    Get(addr3)
  //  ))
}
