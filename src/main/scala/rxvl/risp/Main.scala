package rxvl.risp

object Main {
  def main(args: Array[String]): Unit = {

    def loop(): Unit = {
      val line = scala.io.StdIn.readLine()
      if (line.trim.isEmpty) return
      val parsed = Parser.parse(line)
      println(parsed.fold(
        (a, b, c) => (a, b, c).toString,
        (e, _) => Evaluator.evalE(e).toString
      ))
      loop()
    }
    loop()
  }
}

