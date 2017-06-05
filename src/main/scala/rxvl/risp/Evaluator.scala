package rxvl.risp

object Evaluator {
  import model._
  import matryoshka._
  import scalaz.State
  import scalaz.std.vector.vectorInstance
  import scalaz.syntax.traverse.ToTraverseOpsUnapply

  sealed trait EvalResult
  case class Error(msg: String) extends EvalResult
  case class IResult(i: Int) extends EvalResult
  case class BResult(b: Boolean) extends EvalResult

  type Memo[T] = State[Map[String, Int], T]

  def eval: Algebra[ExprF, Memo[EvalResult]] = {
    case NumberF(i) => State.init.map(_ => IResult(i))
    case BoolF(b) => State.init.map(_ => BResult(b))
    case SymbolF(name) => State.gets(s => IResult(s.apply(name)))
    case IfF(cond, ifTrue, ifFalse) =>
      cond.flatMap {
        case Error(_) => cond
        case BResult(true) => ifTrue
        case BResult(false) => ifFalse
        case IResult(x) => State.gets(_ => Error(s"$x wasn't a boolean"))
      }
    case DefF(label, expr) => for {
      result <- expr
      _ <- result match {
        case IResult(r) => State.modify[Map[String, Int]](_ + (label -> r))
        case _ => expr
      }
    } yield result
    case AppF("begin", args) =>
      args.sequence[Memo, EvalResult].map(_.last)
    case AppF("add", args) =>
      args.sequence[Memo, EvalResult].map(a =>
        IResult(a.map { case IResult(i) => i; case _ => ??? }.sum))
    case AppF("lt", args) =>
      args.sequence[Memo, EvalResult].map(a => {
        val Vector(a1, a2) = a.map { case IResult(i) => i; case _ => ??? }
        BResult(a1 < a2)
      })
    case AppF("eq", args) =>
      args.sequence[Memo, EvalResult].map(a => {
        val Vector(a1, a2) = a.map { case IResult(i) => i; case _ => ??? }
        BResult(a1 == a2)
      })
    case AppF("mult", args) =>
      args.sequence[Memo, EvalResult].map(a =>
        IResult(a.map { case IResult(i) => i; case _ => ??? }.product))
    case FnF(_, _) => ???
    case AppF(_, args) => ???
  }

  def evalE(e: Expr) = {
    val f = Recursive[Expr, ExprF].cata[State[Map[String, Int], EvalResult]](e)(eval)
    f.run(Map())
  }

}
