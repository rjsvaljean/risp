package rxvl.risp

object Evaluator {
  import model._
  import matryoshka._
  import scalaz.State

  sealed trait EvalResult
  case class TypeError(msg: String) extends EvalResult
  case class Result(i: Int) extends EvalResult

  // (begin (define))

  def eval: Algebra[ExprF, State[Map[String, Int], EvalResult]] = {
    case NumberF( i ) => State.init.map(_ => Result(i))
    case SymbolF( name ) => State.gets(s => Result(s.apply(name)))
    case IfF( cond, ifTrue, ifFalse ) =>
      cond.flatMap {
        case TypeError(_) => cond
        case Result(0) => ifFalse
        case Result(1) => ifTrue
        case Result(x) => State.gets(_ => TypeError(s"$x can't be boolified"))
      }
//    case FnF( parameters, body ) => FnDef
    case DefF( label, expr ) => for {
      result <- expr
      _ <- result match {
        case Result(r) => State.modify[Map[String, Int]](_ + (label -> r))
        case TypeError(_) => expr
      }
    } yield result
//    case DefF( label, _ ) => store.put(label, )
//    case AppF( fn, args ) =>
  }

  def evalE(e: Expr) = {
    val f = Recursive[Expr, ExprF].cata[State[Map[String, Int], EvalResult]](e)(eval)
    f.run(Map())
  }

}
