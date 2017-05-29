package rxvl.risp

package rjs

import scalaz.{Traverse, Applicative, Monoid, State, Functor}

sealed trait AST[+A]

case class ALambda[A](arg: String, lam: A) extends AST[A]

case class AApply[A](abstractExpr: A, appliedTo: A) extends AST[A]

case class ANumber(i: Int) extends AST[Nothing]

case class AString(s: String) extends AST[Nothing]

case class AIdent(id: String) extends AST[Nothing]

case class Mu[F[_]](value: F[Mu[F]])

object AST {
  type Unfixed = Mu[AST]

  def example: Unfixed = Mu[AST](AApply(
    Mu[AST](ALambda("x", Mu[AST](AIdent("x")))),
    Mu[AST](ANumber(2))
  ))

  implicit val traversable: Traverse[AST] = new Traverse[AST] {
    def traverseImpl[G[_], A, B](
                                  fa: AST[A]
                                )(
                                  f: (A) => G[B]
                                )(
                                  implicit ap: Applicative[G]
                                ): G[AST[B]] = {
      fa match {
        case n@ANumber(_) => ap.pure(n)
        case s@AString(_) => ap.pure(s)
        case id@AIdent(_) => ap.pure(id)
        case AApply(abs, appliedTo) =>
          ap.apply2(f(abs), f(appliedTo))(AApply(_, _))
        case ALambda(arg, lam) =>
          ap.apply2(ap.pure(arg), f(lam))(ALambda(_, _))
      }
    }

  }
}

sealed trait Type

case class TLambda(t1: Type, t2: Type) extends Type

case class TVar(i: Int) extends Type

case object TNumber extends Type

case object TString extends Type

sealed trait Constraint

// A Type Constraint
case class EqualityConstraint(t1: Type, t2: Type) extends Constraint

case class TypeResult(constraints: List[Constraint], assumptions: Map[String, List[Type]])

object TypeResult {
  implicit val monoid: Monoid[TypeResult] = new Monoid[TypeResult] {
    def zero: TypeResult = TypeResult(Nil, Map())

    def append(x: TypeResult, y: => TypeResult): TypeResult =
      TypeResult(
        constraints = x.constraints ::: y.constraints,
        assumptions = {
          x.assumptions.foldLeft(y.assumptions) { case (acc, (k, vs)) =>
            acc + (k -> acc.get(k).fold(vs)(_ ::: vs))
          }
        }
      )
  }
}

case class TypeState[T, M](varId: Int, memo: Map[T, M])

object TypeCheck {
  type TYPE[T] = State[TypeState[T, (Type, TypeResult)], (Type, TypeResult)]

}

case class CoFree[S[_] : Functor, A](head: A, tail: S[CoFree[S, A]]) {
  def functor = Functor[S]

  final def extract: A = head

  final def map[B](f: A => B): CoFree[S, B] =
    applyCoFree(f, _ map f)

  /** Redecorates this structure with a computation whose context is the entire structure under that value. */
  final def extend[B](f: CoFree[S, A] => B): CoFree[S, B] =
    applyTail(f(this), _ extend f)

  /** Replaces the head with `b` and applies `g` through the tail. */
  final def applyTail[B](b: B, g: CoFree[S, A] => CoFree[S, B]): CoFree[S, B] =
    applyCoFree(x => b, g)

  /** Applies `f` to the head and `g` through the tail. */
  final def applyCoFree[B](f: A => B, g: CoFree[S, A] => CoFree[S, B]): CoFree[S, B] =
    CoFree(f(head), functor.map(tail)(g))

}

object CoFree {
  def cofreeMu[F[_] : Functor](muf: Mu[F]): CoFree[F, Unit] = {
    val Mu(f) = muf
    CoFree[F, Unit]((), Functor[F].map(f)(cofreeMu(_)(Functor[F])))
  }

  implicit def traversable[S[_] : Traverse]: Traverse[({type l[a] = CoFree[S, a]})#l] =
    new Traverse[({type l[a] = CoFree[S, a]})#l] {
      def traverseImpl[G[_], A, B](
                                    fa: CoFree[S, A]
                                  )(
                                    f: (A) => G[B]
                                  )(
                                    implicit ap: Applicative[G]
                                  ): G[CoFree[S, B]] = {
        val ga: G[S[CoFree[S, B]]] = Traverse[S].traverse(fa.tail)(traverse(_)(f))
        val gb: G[B] = f(fa.head)
        ap.apply2(gb, ga)(CoFree[S, B])
      }
    }
}

object Blah {

  import scalaz.State
  import scalaz.syntax.semigroup._

  def main: Unit = {
    println(CoFree.cofreeMu(AST.example))
    println(attribute(CoFree.cofreeMu(AST.example)))
    println(typeTree(CoFree.cofreeMu(AST.example)))
  }

  def freshVarId[T, M]: State[TypeState[T, M], Type] =
    for {
      v <- State.gets((_: TypeState[T, M]).varId)
      _ <- State.modify((ts: TypeState[T, M]) => ts.copy(varId = ts.varId + 1))
    } yield TVar(v)


  def memoizedTC[C](f: C => TypeCheck.TYPE[C], c: C): TypeCheck.TYPE[C] = {
    def memoize: State[TypeState[C, (Type, TypeResult)], (Type, TypeResult)] = for {
      r <- f(c)
      _ <- State.modify((ts: TypeState[C, (Type, TypeResult)]) => ts.copy(memo = ts.memo + (c -> r)))
    } yield r

    State.gets((_: TypeState[C, (Type, TypeResult)]).memo).
      flatMap(_.get(c).fold(memoize)(State.state))
  }

  def generateConstraints(c: CoFree[AST, Unit]): TypeCheck.TYPE[CoFree[AST, Unit]] = {
    c.tail match {
      case ANumber(_) => State.state((TNumber, Monoid[TypeResult].zero))
      case AString(_) => State.state((TString, Monoid[TypeResult].zero))
      case AIdent(s) => for {
        vaar <- freshVarId
      } yield (vaar, TypeResult(Nil, Map(s -> List(vaar))))
      case ALambda(s, b) => for {
        vaar <- freshVarId
        br <- memoizedTC(generateConstraints, b)
        cs = br._2.assumptions.get(s).fold(List[Constraint]())(_.map(EqualityConstraint(vaar, _)))
        as = br._2.assumptions - s
      } yield (
        TLambda(vaar, br._1): Type,
        TypeResult(br._2.constraints ::: cs, Map(s -> List(vaar)))
      )
      case AApply(a, b) => for {
        vaar <- freshVarId
        ar <- memoizedTC(generateConstraints, a)
        br <- memoizedTC(generateConstraints, b)
      } yield (
        vaar,
        ar._2 |+| br._2 |+| TypeResult(List(EqualityConstraint(ar._1, TLambda(br._1, vaar))), Map())
      )
    }
  }

  def attribute(c: CoFree[AST, Unit]): CoFree[AST, (Type, TypeResult)] = {
    val initial = TypeState[CoFree[AST, Unit], (Type, TypeResult)](memo = Map(), varId = 0)

    Traverse[({type l[a] = CoFree[AST, a]})#l].
      sequence[
      ({type l[a] = State[TypeState[CoFree[AST, Unit], (Type, TypeResult)], a]})#l,
      (Type, TypeResult)
      ](c.extend(memoizedTC[CoFree[AST, Unit]](generateConstraints, _))).
      run(initial).
      _2
  }

  val solveConstraints: List[Constraint] => Option[Map[Int, Type]] = _.foldLeft(
    Option(Map[Int, Type]())
  ) { (b: Option[Map[Int, Type]], a: Constraint) =>
    def solve(maybeSubs: Option[Map[Int, Type]], const: Constraint) = {
      const match {
        case EqualityConstraint(_a, _b) => for {
          subs <- maybeSubs
          t <- mostGeneralUnifier(substitute(subs, _a), substitute(subs, _b))
        } yield t
      }
    }

    scalaz.std.option.optionInstance.apply2(solve(b, a), b)(_ ++ _)
  }


  private def mostGeneralUnifier(t1: Type, t2: Type): Option[Map[Int, Type]] = {
    (t1, t2) match {
      case (TVar(i), b) => Some(Map(i -> b))
      case (a, TVar(i)) => Some(Map(i -> a))
      case (TNumber, TNumber) => Some(Map())
      case (TString, TString) => Some(Map())
      case (TLambda(a, b), TLambda(c, d)) => for {
        s1 <- mostGeneralUnifier(a, c)
        o <- mostGeneralUnifier(substitute(s1, b), substitute(s1, d))
      } yield s1 ++ o
      case _ => None
    }
  }

  private def substitute(m: Map[Int, Type], t: Type): Type = (m, t) match {
    case (subs, v@TVar(i)) => subs.get(i).fold(v: Type)(substitute(subs, _))
    case (subs, TLambda(a, b)) => TLambda(substitute(subs, a), substitute(subs, b))
    case (_, t) => t
  }

  def typeTree(c: CoFree[AST, Unit]): Option[CoFree[AST, Type]] = {
    val result = attribute(c)
    val r = result.head
    val constraints = r._2.constraints
    val maybeSubs = solveConstraints(constraints)
    maybeSubs.map(subs => result.map(r => substitute(subs, r._1)))
  }
}