package rxvl.risp

object Evaluator {
  import model._
  import matryoshka._
  import scalaz.{State, Traverse}
  import scalaz.std.vector.vectorInstance
  import scalaz.syntax.traverse.ToTraverseOpsUnapply

  sealed trait EvalResult
  case class Error(msg: String) extends EvalResult
  case class Result(i: Int) extends EvalResult

  type Memo[T] = State[Map[String, Int], T]

  def eval: Algebra[ExprF, Memo[EvalResult]] = {
    case NumberF( i ) => State.init.map(_ => Result(i))
    case SymbolF( name ) => State.gets(s => Result(s.apply(name)))
    case IfF( cond, ifTrue, ifFalse ) =>
      cond.flatMap {
        case Error(_) => cond
        case Result(0) => ifFalse
        case Result(1) => ifTrue
        case Result(x) => State.gets(_ => Error(s"$x can't be boolified"))
      }
    case DefF( label, expr ) => for {
      result <- expr
      _ <- result match {
        case Result(r) => State.modify[Map[String, Int]](_ + (label -> r))
        case Error(_) => expr
      }
    } yield result
    case AppF( "begin", args ) =>
      args.sequence[Memo, EvalResult].map(_.last)
    case AppF( "add", args ) =>
      args.sequence[Memo, EvalResult].map(a =>
        Result(a.map { case Result(i) => i; case _ => ??? }.sum)
      )
    case AppF( "mult", args ) =>
      args.sequence[Memo, EvalResult].map(a =>
        Result(a.map { case Result(i) => i; case _ => ??? }.product)
      )
    case FnF( _, _ ) => ???
    case AppF( _, args ) => ???
  }

  def evalE(e: Expr) = {
    val f = Recursive[Expr, ExprF].cata[State[Map[String, Int], EvalResult]](e)(eval)
    f.run(Map())
  }

}
