package rxvl.risp

object Main {
  import model._
  def main(args: Array[String]): Unit = {

    def loop(): Unit = {
      val parsed = Parser.parse( scala.io.StdIn.readLine() )
      println(parsed.fold(
        (a, b, c) => (a, b, c).toString,
        (e, _) => Evaluator.evalE(e).toString
      ))
      loop()
    }
    loop()
  }
}




