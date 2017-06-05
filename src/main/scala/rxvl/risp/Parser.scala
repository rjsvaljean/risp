package rxvl.risp

import matryoshka.data.Fix

object Parser {
  import fastparse.all._
  import model._

  val NumberFParser: P[NumberF] =
    P(CharIn('0' to '9').!.map(i => NumberF(i.toInt)))
  val BoolFParser: P[BoolF] =
    P(("true".!.map(_ => true) | "true".!.map(_ => false)).map(BoolF(_)))
  val SymbolFParser: P[SymbolF] =
    P(CharIn('a' to 'z').!.map(s => SymbolF(s)))
  def IfExprFParser[A](pa: => P[A]): P[IfF[A]] =
    P(("(if " ~ pa ~ " " ~ pa ~ " " ~ pa ~ ")").map((IfF[A] _).tupled))
  def FnFParser[A](pa: => P[A]): P[FnF[A]] = {
    val argument = P(CharIn('a' to 'z').!.map(_.toString))
    val arguments = P(argument.rep(sep = " "))
    P(("(fn (" ~ arguments ~ ") " ~ pa ~ ")").map {
      case (args, f) => FnF(args.toVector, f)
    })
  }
  def DefFParser[A](pa: => P[A]): P[DefF[A]] = {
    val name = P(CharIn('a' to 'z').!.map(_.toString))
    P(("(define " ~ name ~ " " ~ pa ~ ")").map {
      case (n, body) => DefF(n, body)
    })
  }
  def AppFParser[A](pa: => P[A]): P[AppF[A]] = {
    val fnName = P(CharIn('a' to 'z').rep.!.map(_.toString))
    P(("(" ~ fnName ~ " " ~ pa.rep(sep = " ") ~ ")").map {
      case (name, args) => AppF(name, args.toVector)
    })
  }
  def pfp[A](pa: => P[A]): P[ExprF[A]] = NumberFParser |
    BoolFParser |
    SymbolFParser |
    IfExprFParser(pa) |
    FnFParser(pa) |
    DefFParser(pa) |
    AppFParser(pa)

  def unfixed(pe: => P[Expr]) = pfp[Expr](pe).map(_.fix)
  val parser = scalaz.Scalaz.fix(unfixed)
  def parse(str: String): Parsed[Expr] = parser.parse(str)
}
