import cats.effect.IO
object Main extends App{
  def putStrlLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)
  def runCalcs(): IO[String] = {
    def getNextInpt(msg: String): IO[String] = {
      for {
        _ <- putStrlLn(msg)
        _ <- putStrlLn(
          "What would you like to calculate next?\n " +
            "('end' to end program)"
        )
        next <- readLn
      } yield next
    }
    def run_while(inpt: String): IO[String] = {
      if(inpt.stripLineEnd == "end")
        for{
          _ <- putStrlLn("Goodbye")
          v <- IO("")
        } yield v
      else{
        val expNum: Num = Expr(inpt.toList)
        val retVal: Maybe[Double] = expNum.eval
        val nextInpt: IO[String] = retVal match{
          case Result(d) =>
            getNextInpt(
              s"$inpt = $d"
            )
          case Raise(err) =>
            getNextInpt(
              s"The computation $inpt resulted in error:\n$err."
            )
        }
        nextInpt.flatMap(run_while)
      }
    }
    val startComp: IO[String] = for {
      _ <- putStrlLn("What would you like to compute?")
      input <- readLn
    } yield input
    startComp.flatMap(run_while)
  }
  runCalcs().unsafeRunSync()
}