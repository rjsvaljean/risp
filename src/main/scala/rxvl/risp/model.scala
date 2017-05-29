package rxvl.risp

object model {
  import matryoshka.data.Fix

  import scalaz.Functor

  sealed trait ExprF[+A] {
    def fix[AA >: A](implicit ev: AA =:= Fix[ExprF]) = Fix[ExprF](this.asInstanceOf[ExprF[Fix[ExprF]]])
  }

  case class NumberF(i: Int) extends ExprF[Nothing]
  case class BoolF(b: Boolean) extends ExprF[Nothing]

  case class SymbolF(name: String) extends ExprF[Nothing]

  case class IfF[+A](
    cond: A,
    ifTrue: A,
    ifFalse: A
  ) extends ExprF[A]

  case class FnF[+A](
    parameters: Vector[String],
    body: A
  ) extends ExprF[A]

  case class DefF[+A](
    label: String,
    body: A
  ) extends ExprF[A]

  case class AppF[+A](
    fn: String,
    args: Vector[A]
  ) extends ExprF[A]

  implicit object functor extends Functor[ExprF] {
    def map[ A, B ](
      fa: ExprF[ A ]
    )(
      f: ( A ) => B
    ): ExprF[ B ] = fa match {
      case NumberF( i ) => NumberF(i)
      case BoolF( i ) => BoolF(i)
      case SymbolF( name ) => SymbolF(name)
      case IfF( cond, ifTrue, ifFalse ) =>
        IfF(f(cond), f(ifTrue), f(ifFalse))
      case FnF( parameters, body ) =>
        FnF(parameters, f(body))
      case DefF( label, body ) =>
        DefF(label, f(body))
      case AppF( fn, args ) =>
        AppF(fn, args.map(f))
    }
  }

  type Expr = Fix[ExprF]

  object Expr {
    def Number(i: Int): Expr = Fix[ExprF](NumberF(i))
    def Bool(b: Boolean): Expr = Fix[ExprF](BoolF(b))

    def Symbol(name: String): Expr =
      Fix[ExprF](SymbolF(name))

    def If(
      cond: Expr,
      ifTrue: Expr,
      ifFalse: Expr
    ): Expr = Fix[ExprF](IfF(cond, ifTrue, ifFalse))

    def Fn(
      parameters: Vector[String],
      body: Expr
    ): Expr = Fix[ExprF](FnF(parameters, body))

    def Def(
      label: String,
      body: Expr
    ): Expr = Fix[ExprF](DefF(label, body))

    def App(
      fn: String,
      args: Vector[Expr]
    ): Expr = Fix[ExprF](AppF(fn, args))
  }


}
