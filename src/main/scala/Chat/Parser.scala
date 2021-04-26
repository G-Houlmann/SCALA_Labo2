package Chat

import Chat.Tokens._
import Tree._
import Data.Products

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
      if (curToken == VOULOIR) {
        readToken()
        if (curToken == COMMANDER) {
          readToken()
          Command(parseItems())
        } else if (curToken == CONNAITRE) {
          readToken()
          parseBalance()
        } else expected(COMMANDER, CONNAITRE)
      } else if (curToken == MOI) {
        eat(APPELER)
        Identification(stripName(curValue))
      } else {
        eat(ETRE)
        if (curToken == AFFAME) {
          Hungry()
        } else if (curToken == ASSOIFFE) {
          Thirsty()
        } else if (curToken == PSEUDO) {
          Identification(stripName(curValue))
        } else {
          expected(AFFAME, ASSOIFFE, PSEUDO)
        }
      }
    }
    else {
      parsePrice()
    }
  }

  /** Remove underscore before username */
  private def stripName(name: String): String = {
    name.replace("_", "")
  }

  /** Parse the balance request */
  private def parseBalance() : ExprTree = {
    eat(MOI)
    eat(SOLDE)
    Balance()
  }

  /** Parse a price request */
  private def parsePrice() : ExprTree = {
    if (curToken == COMBIEN) {
      readToken()
      eat(COUTER)
    }else if (curToken == QUEL) {
      readToken()
      eat(ETRE)
      eat(USELESS)
      eat(PRIX)
      eat(USELESS)
    } else expected(COMBIEN, QUEL)

    Price(parseItems())
  }

  /** Parse items in a command/price request */
  private def parseItems(): ExprTree = {
    val item = parseProduct()

    // Use this little trick to build the tree from
    // the bottom and gain left precedence
    def loop(rexp: ExprTree): ExprTree = {
      if (curToken == ET) {
        readToken()
        loop(And(parseProduct(), rexp))
      } else if (curToken == OU) {
        readToken()
        loop(Or(parseProduct(),rexp))
      } else {
        rexp
      }
    }

    loop(item)
  }

  /** Parse an independent item */
  private def parseProduct(): ExprTree = {
    var product: Products.Product = 0
    var quantity = 0

    if (curToken == NUM) {
      quantity = Integer.parseInt(curValue)
      readToken()
    } else expected(NUM)

    if (curToken == CROISSANT) {
      product = Products.CROISSANT
      readToken()
    } else if (curToken == BIERE) {
      product = Products.BEER
      readToken()
    } else expected(CROISSANT, BIERE)

    if (curToken == MARQUE) {
      val brand = curValue
      readToken()
      Item(product, brand, quantity)
    } else {
      DefaultItem(product, quantity)
    }
  }

  // Start the process by reading the first token.
  readToken()
}
