package rxvl.ntt

import org.scalatest.{ FlatSpec, Matchers }
import rxvl.ntt.BinHelpers.toAddr

class AssemblyParserSpec extends FlatSpec with Matchers {
  behavior of "AssemblyParser"

  it should "be able to parse the 1 + ... + 100 program" in {

    sampleProgram.zip(parsedSampleProgram).foreach {
      case (line, parsedLine) =>
        CPU.AssemblyParser.parser.parse(line).get.value should be(parsedLine)
    }
  }

  val sampleProgram = List(
    "@i",
    "M=1",
    "@sum",
    "M=0",
    "(LOOP)",
    "@i",
    "D=M",
    "@100",
    "D=D-A",
    "@END",
    "D;JGT",
    "@i",
    "D=M",
    "@sum",
    "M=D+M",
    "@i",
    "M=M+1",
    "@LOOP",
    "0;JMP",
    "(END)",
    "@END",
    "0;JMP"
  )

  val parsedSampleProgram = {

    import CPU._
    import ALURegister._
    import ALUOp._
    import Symbols._
    import JumpOp._

    List(
      AtS(Variable("i")), // @i // i refers to some mem. location.
      Pure(Ci(Just(One), Set(M))), // M=1 // i=1
      AtS(Variable("sum")), // @sum // sum refers to some mem. location.
      Pure(Ci(Just(Zero), Set(M))), // M=0 // sum=0
      LabelDeclS(Label("LOOP")), // (LOOP)
      AtS(Variable("i")), // @i
      Pure(Ci(Just(M), Set(D))), // D=M // D=i
      Pure(At(toAddr(100))), // @100
      Pure(Ci(Minus(D, A), Set(D))), // D=D-A // D=i-100
      AtS(Variable("END")), // @END
      Pure(Ci(Just(D), Set(), JGT)), // D;JGT // If (i-100)>0 goto END
      AtS(Variable("i")), // @i
      Pure(Ci(Just(M), Set(D))), // D=M // D=i
      AtS(Variable("sum")), // @sum
      Pure(Ci(Plus(D, M), Set(M))), // M=D+M // sum=sum+i
      AtS(Variable("i")), // @i
      Pure(Ci(Plus(M, One), Set(M))), // M=M+1 // i=i+1
      AtS(Variable("LOOP")), // @LOOP
      Pure(Ci(Just(Zero), Set(), JMP)), // 0;JMP // Goto LOOP
      LabelDeclS(Label("END")), // (END)
      AtS(Variable("END")), // @END
      Pure(Ci(Just(Zero), Set(), JMP)) // 0;JMP // Infinite loop
    )
  }

}
