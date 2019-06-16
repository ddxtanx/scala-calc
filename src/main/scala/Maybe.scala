class Maybe[A]{
  def apply(a: => A): Maybe[A] = Result(a)
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Result(av) => f(av)
    case Raise(e) => Raise(e)
  }
  def comb(a2: Maybe[A], f: (A, A) => A): Maybe[A] = {
    this.flatMap(v1 => {
      a2.flatMap(v2 => {
        Result(f(v1, v2))
      })
    })
  }
}
case class Result[A](a: A) extends Maybe[A]
case class Raise[A](e: String) extends Maybe[A]