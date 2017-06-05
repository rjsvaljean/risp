package rxvl.risp

object Compiler {

  import matryoshka.{ Algebra, Recursive }
  import rxvl.risp.model.{ Expr, ExprF }
  import rxvl.ntt.CPU
  import rxvl.ntt.Word
  import rxvl.ntt.Memory.Address
  import rxvl.ntt.ArithematicGates.toBin
  import rxvl.ntt.ArithematicGates.fromBin
  import rxvl.ntt.Memory.Byte16
  import rxvl.risp.model._

  import scalaz.State
  import shapeless.nat.{ _9, _16 }

  type Memo[T] = State[Map[String, Int], T]

  val reg1: Address = Word[_9, Boolean](Vector.fill(9)(true).dropRight(2) ++ Vector(true, true))

  def PutOut(i: Int) = CPU.Put(reg1, toBin(i))

  case class CompilerState(
    private val symbol: Map[String, Address] = Map()
  )

  def eval: Algebra[ExprF, State[CompilerState, Seq[CPU.Op]]] = {
    case NumberF(i) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq(PutOut(i)))
    case BoolF(b) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case SymbolF(name) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case IfF(cond, ifTrue, ifFalse) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case DefF(label, expr) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case AppF("begin", args) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case AppF("add", args) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case AppF("lt", args) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case AppF("eq", args) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case AppF("mult", args) => State.gets[CompilerState, Seq[CPU.Op]](_ => Seq())
    case _ => ???
  }

  def evalE(e: Expr) = {
    val f = Recursive[Expr, ExprF].cata[State[CompilerState, Seq[CPU.Op]]](e)(eval)
    CPU.runScript(f.run(CompilerState())._2 :+ CPU.Get(reg1))
  }

}
