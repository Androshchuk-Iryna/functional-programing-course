package kse.unit7.topic

object adt:

  enum Option[+V]:

    case Some(x: V) extends Option[V]
    case None       extends Option[Nothing]

    def flatMap[Q](f: V => Option[Q]): Option[Q] =
      this match
        case Option.None    => Option.None
        case Option.Some(v) => f(v)

    def map[Q](f: V => Q): Option[Q] =
      this match
        case Option.None    => Option.None
        case Option.Some(v) => Option.Some(f(v))

  object Option:

    def apply[V](v: V): Option[V] =
      if v == null then Option.None else Option.Some(v)

  def numeral[A](n: Int): (A => A) => A => A = {
    f => x => {
      if (n == 0) x
      else (1 to n).foldLeft(x)((acc, _) => f(acc))
    }
  }
  def numeral[A](n: Int): (A => A) => A => A =
    f => x =>
      if (n == 0) x
      else f(numeral[A](n-1)(f)(x))
