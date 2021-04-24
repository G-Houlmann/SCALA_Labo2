import Chat.{Parser, Tokenizer, UnexpectedTokenException}
import Utils.ClinksCalculator._

import scala.io.StdIn

object Main extends App {
  println("Bienvenue au Chill-Out !")

  while (true) {
    print("> ")
    StdIn.readLine.toLowerCase match {
      case "quitter" => println("Adieu."); System.exit(0)
      case "santé !" => {
        for (i <- 2 to 6) {
          println(s"Nombre de *clinks* pour un santé de $i personnes : ${calculateCombination(i, 2)}.")
        }
      }
      case s => {
        try {
          val tokenizer = new Tokenizer(s)
          tokenizer.tokenize()

          val parser = new Parser(tokenizer)
          val expr = parser.parsePhrases()
          val printResult = expr.reply

          println(printResult)
        } catch {
          case e: UnexpectedTokenException => println(s"Invalid input. ${e.getMessage}")
        }
      }
    }
  }
}
