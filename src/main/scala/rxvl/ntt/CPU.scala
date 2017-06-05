package rxvl.ntt

import Memory._
import rxvl.ntt.CPU.ALURegister._
import rxvl.ntt.BinHelpers.toAddr

import scalaz.State
import shapeless.nat._

object CPU {

  sealed trait HackOp {
    def toByte16: Byte16
  }

  sealed trait ALUOp
  sealed trait ALURegister {
    def fold(a: Byte16, m: Byte16, d: Byte16): Byte16 = this match {
      case Zero => Word[_16](false)
      case One => Word[_16, Boolean](Vector.fill(15)(false) :+ true)
      case D => d
      case M => m
      case A => a
    }
  }
  object ALURegister {
    case object Zero extends ALURegister
    case object One extends ALURegister
    case object D extends ALURegister
    case object M extends ALURegister
    case object A extends ALURegister

    def toSet(dest: Word[_3, Boolean]): Set[ALURegister] =
      (if (dest.bits(0)) Set(ALURegister.A) else Set()) ++
        (if (dest.bits(1)) Set(ALURegister.D) else Set()) ++
        (if (dest.bits(2)) Set(ALURegister.M) else Set())

    def fromSet(registers: Set[ALURegister]): Word[_3, Boolean] =
      Word[_3, Boolean](Vector(
        if (registers.contains(ALURegister.A)) true else false,
        if (registers.contains(ALURegister.D)) true else false,
        if (registers.contains(ALURegister.M)) true else false
      ))

    def fromChar(reg: String): ALURegister = reg match {
      case "A" => A
      case "M" => M
      case "D" => D
      case "0" => Zero
      case "1" => One
    }
  }
  object ALUOp {
    case class Just(operand: ALURegister) extends ALUOp
    case class Negate(operand: ALURegister) extends ALUOp
    case class Not(operand: ALURegister) extends ALUOp
    case class Plus(operandl: ALURegister, operandr: ALURegister) extends ALUOp
    case class Minus(operandl: ALURegister, operandr: ALURegister) extends ALUOp
    case class BitAnd(operandl: ALURegister, operandr: ALURegister) extends ALUOp
    case class BitOr(operandl: ALURegister, operandr: ALURegister) extends ALUOp
  }
  sealed trait JumpOp
  object JumpOp {
    case object NoJump extends JumpOp
    case object JGT extends JumpOp
    case object JEQ extends JumpOp
    case object JGE extends JumpOp
    case object JLT extends JumpOp
    case object JNE extends JumpOp
    case object JLE extends JumpOp
    case object JMP extends JumpOp

    val fromString: Map[String, JumpOp] = Map(
      "JGT" -> JGT,
      "JEQ" -> JEQ,
      "JGE" -> JGE,
      "JLT" -> JLT,
      "JNE" -> JNE,
      "JLE" -> JLE,
      "JMP" -> JMP
    )

    private val map: Map[Vector[Boolean], JumpOp] = Map(
      Vector(false, false, false) -> JumpOp.NoJump,
      Vector(false, false, true) -> JumpOp.JGT,
      Vector(false, true, false) -> JumpOp.JEQ,
      Vector(false, true, true) -> JumpOp.JGE,
      Vector(true, false, false) -> JumpOp.JLT,
      Vector(true, false, true) -> JumpOp.JNE,
      Vector(true, true, false) -> JumpOp.JLE,
      Vector(true, true, true) -> JumpOp.JMP
    )
    private val reverseMap: Map[JumpOp, Vector[Boolean]] = map.map(_.swap)

    def apply(w: Word[_3, Boolean]): JumpOp = map(w.bits)
    def toBits(op: JumpOp): Word[_3, Boolean] = Word[_3, Boolean](reverseMap(op))
  }

  case class At(addr: Address) extends HackOp {
    val toByte16 = Word[_16, Boolean](false +: addr.bits)
  }

  class Comp(a: Boolean, cs: Word[_6, Boolean]) {
    val bits = a +: cs.bits
    val toALUOp: ALUOp = Comp.boolToALUOp(bits)
  }

  object Comp {
    import ALUOp._
    import ALURegister._

    private def map(a: Boolean): Seq[(Vector[Boolean], ALUOp)] = {
      val aOrM = if (a) A else M

      Vector(
        Vector(a, true, false, true, false, true, false) -> Just(Zero),
        Vector(a, true, true, true, true, true, true) -> Just(One),
        Vector(a, true, true, true, false, true, false) -> Negate(One),
        Vector(a, false, false, true, true, false, false) -> Just(D),
        Vector(a, false, false, true, true, false, false) -> Just(aOrM),
        Vector(a, false, false, true, true, false, true) -> Not(D),
        Vector(a, true, true, false, false, false, true) -> Not(aOrM),
        Vector(a, false, false, true, true, true, true) -> Negate(D),
        Vector(a, true, true, false, false, true, true) -> Negate(aOrM),
        Vector(a, false, true, true, true, true, true) -> Plus(D, One),
        Vector(a, true, true, false, true, true, true) -> Plus(aOrM, One),
        Vector(a, false, false, true, true, true, false) -> Minus(D, One),
        Vector(a, true, true, false, false, true, false) -> Minus(aOrM, One),
        Vector(a, false, false, false, false, true, false) -> Plus(D, aOrM),
        Vector(a, false, true, false, false, true, true) -> Minus(D, aOrM),
        Vector(a, false, false, false, true, true, true) -> Minus(aOrM, D),
        Vector(a, false, false, false, false, false, false) -> BitAnd(D, aOrM),
        Vector(a, false, true, false, true, false, true) -> BitOr(D, aOrM)
      )
    }
    val boolToALUOp: Map[Vector[Boolean], ALUOp] = (map(true) ++ map(false)).toMap
    val aluOpToBool: Map[ALUOp, Vector[Boolean]] = (map(true) ++ map(false)).map(_.swap).toMap

    def apply(aluOp: ALUOp): Comp = {
      val vector = aluOpToBool(aluOp)
      new Comp(vector.head, Word[_6, Boolean](vector.tail))
    }
  }

  class Ci(
      val comp: Comp,
      dest: Word[_3, Boolean],
      jump: Word[_3, Boolean]
  ) extends HackOp {
    val toByte16 = Word[_16, Boolean](Vector(true, true, true) ++ comp.bits ++ dest.bits ++ jump.bits)
    val destinationRegisters: Set[ALURegister] = ALURegister.toSet(dest)
    val jumpOp: JumpOp = JumpOp(jump)

    override def toString = this match {
      case Ci(comp_, dest_, jump_) => s"Ci($comp_, $dest_, $jump_)"
    }

    override def equals(obj: scala.Any) = (this, obj) match {
      case (Ci(comp1, dest1, jump1), Ci(comp2, dest2, jump2)) =>
        (comp1 == comp2) && (dest1 == dest2) && (jump1 == jump2)
      case _ => false
    }
  }

  object Ci {
    def apply(
      aluOp: ALUOp,
      aluRegisters: Set[ALURegister],
      jumpOp: JumpOp = JumpOp.NoJump
    ): Ci = new Ci(Comp(aluOp), ALURegister.fromSet(aluRegisters), JumpOp.toBits(jumpOp))

    def unapply(arg: Ci): Option[(ALUOp, Set[ALURegister], JumpOp)] =
      Some((arg.comp.toALUOp, arg.destinationRegisters, arg.jumpOp))
  }

  case class LabelDecl(address: Address) extends HackOp {
    def toByte16 = ???
  }

  case class JumpDeclaration(symbol: String)
  case class JumpStatement(symbol: String)

  type MainMemory = Vector[Vector[Vector[Vector[Vector[Byte16]]]]]
  type ProgramMemeory = Vector[Vector[Vector[Vector[Vector[Byte16]]]]]
  type AState = Byte16
  type MState = Byte16
  type DState = Byte16
  case class RegisterState(
      mainMemory: MainMemory = RAM16KInit,
      programMemeory: ProgramMemeory = RAM16KInit,
      aState: AState = Null,
      mState: MState = Null,
      dState: DState = Null
  ) {
    def updateRegisters(registers: Set[ALURegister], out: Byte16) =
      registers.foldLeft(this) {
        case (_rs, ALURegister.A) => _rs.copy(aState = out)
        case (_rs, ALURegister.M) => _rs.copy(mState = out)
        case (_rs, ALURegister.D) => _rs.copy(dState = out)
        case (_rs, _D) => _rs
      }
  }

  type CPUState[A] = State[RegisterState, A]

  val run: HackOp => CPUState[Unit] = {
    case At(addr) => State.modify { rs =>
      val (nrs, a) = RAM16K(load = false, Null, addr).run(rs.mainMemory)
      rs.copy(mainMemory = nrs, aState = a, mState = a)
    }
    case Ci(ALUOp.Plus(r1, r2), outputRegisters, _) => State.modify {
      case rs @ RegisterState(_, _, a, m, d) =>
        val out = ALU.run(ALU.Plus, r1.fold(a, m, d), r2.fold(a, m, d))
        rs.updateRegisters(outputRegisters, out)
    }
    case Ci(ALUOp.Just(r), outputRegisters, _) => State.modify {
      case rs @ RegisterState(_, _, a, m, d) =>
        rs.updateRegisters(outputRegisters, r.fold(a, m, d))
    }
  }

  def runScript(script: Seq[HackOp]): Unit =
    script.map(run).reduce((s1, s2) => s1.flatMap(_ => s2)).run(RegisterState())._2

  sealed trait Symbol
  object Symbols {
    case object R0 extends Symbol
    case object R1 extends Symbol
    case object R2 extends Symbol
    case object R3 extends Symbol
    case object R4 extends Symbol
    case object R5 extends Symbol
    case object R6 extends Symbol
    case object R7 extends Symbol
    case object R8 extends Symbol
    case object R9 extends Symbol
    case object R10 extends Symbol
    case object R11 extends Symbol
    case object R12 extends Symbol
    case object R13 extends Symbol
    case object R14 extends Symbol
    case object R15 extends Symbol

    case object SP extends Symbol
    case object LCL extends Symbol
    case object ARG extends Symbol
    case object THIS extends Symbol
    case object THAT extends Symbol

    case object SCREEN extends Symbol
    case object KBD extends Symbol

    case class Label(str: String) extends Symbol
    case class Variable(str: String) extends Symbol

    def resolve(sym: Symbol): Address = ???

    sealed trait SymbolOp
    case class Pure(hackOp: HackOp) extends SymbolOp
    case class AtS(symbol: Variable) extends SymbolOp
    case class LabelDeclS(label: Label) extends SymbolOp
  }

  object AssemblyParser {
    import fastparse.all._
    import Symbols._

    private val amd = P(CharIn(Seq('A', 'M', 'D', '0', '1'))).!.map(ALURegister.fromChar)
    private val jmp = P(JumpOp.fromString.keys.map(_.!).reduce(_ | _)).map(JumpOp.fromString)
    val atVar = P(CharIn(('a' to 'z') ++ ('A' to 'Z')).rep(1).!).map(Variable).map(AtS)
    val atConst = P(CharIn('0' to '9').rep(1).!).map(_.toInt).map(toAddr).map(At).map(Pure)
    private val op = P("+" | "-" | "&" | "|").!
    private val labelDecl = P("(" ~/ CharIn('A' to 'Z').rep(1).! ~ ")").map(Label).map(LabelDeclS)
    private val ci = P((amd ~ "=").? ~ amd ~ (op ~/ amd).? ~ (";" ~/ jmp).?).map {
      case (lhsOpt, rhsOperand1, rhsOpAndOperand2Opt, jumpOpt) =>
        Ci(rhsOpAndOperand2Opt.fold(ALUOp.Just(rhsOperand1): ALUOp) {
          case ("+", rhsOperand2) => ALUOp.Plus(rhsOperand1, rhsOperand2)
          case ("-", rhsOperand2) => ALUOp.Minus(rhsOperand1, rhsOperand2)
          case ("&", rhsOperand2) => ALUOp.BitAnd(rhsOperand1, rhsOperand2)
          case ("|", rhsOperand2) => ALUOp.BitOr(rhsOperand1, rhsOperand2)
          case (_, _) => ???
        }, lhsOpt.toSet, jumpOpt.getOrElse(JumpOp.NoJump))
    }.map(Pure)
    private val at = P("@" ~/ (atConst | atVar))
    val parser: P[SymbolOp] = P(at | labelDecl | ci)
  }

}
