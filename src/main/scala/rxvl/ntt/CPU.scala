package rxvl.ntt

object CPU {
  import Memory._
  import ArithematicGates.{toBin, fromBin}
  import scalaz._
  import Scalaz._

  def test: Unit = (for {
    _ <- RAM(load = true, toBin(10), addr1)
    inAddr1 <- RAM(load = false, Null, addr1)
    out = ALU.test(ALU.Plus, toBin(20), inAddr1)
    _ <- RAM(load = true, out, addr1)
    savedOut <- RAM(load = false, out, addr1)
  } yield println(fromBin(savedOut))).run(Map())
}
