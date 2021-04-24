package Chat

import Chat.Tokens._
import Tree._

class UnexpectedTokenException(msg: String) extends Exception(msg){}

// TODO - step 4
class Parser(tokenizer: Tokenizer) {
  import tokenizer._

  var curTuple: (String, Token) = ("unknown", UNKNOWN)
  
  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = nextToken()

  /** "Eats" the expected token, or terminates with an error. */
  private def eat(token: Token): Unit = if (token == curToken) readToken() else expected(token)

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  // TODO (BONUS): find a way to display the string value of the tokens (e.g. "BIERE") instead of their integer value (e.g. 6).
  private def expected(token: Token, more: Token*): Nothing = {
    throw new UnexpectedTokenException("Expected: " +
      (token :: more.toList).mkString(" or ") +
      ", found: " + curToken)
  }

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases() : ExprTree = {
    if (curToken == BONJOUR) readToken()
    if (curToken == JE) {
      readToken()
      eat(ETRE)
      if (curToken == ASSOIFFE) { 
        readToken()
        Thirsty()
      }
      else if (curToken == AFFAME) {
        readToken()
        Hungry()
      }
      else expected(ASSOIFFE, AFFAME)
    }
    else expected(BONJOUR, JE)
  }

  // Start the process by reading the first token.
  readToken()
}
