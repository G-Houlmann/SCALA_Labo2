package Chat

import Chat.Tokens._
import Utils.Dictionary.dictionary
import Utils.SpellChecker._

class Tokenizer(input: String) {
  var tokens: Array[(String, Token)] = Array()
  private var currentTokenIndex = -1

  private def getTokenFromString(s: String): Token = s match {
    case "bonjour" => BONJOUR
    case "je" => JE
    case "etre" => ETRE
    case "vouloir" => VOULOIR
    case "et" => ET
    case "ou" => OU
    case "biere" => BIERE
    case "croissant" => CROISSANT
    case "assoiffe" => ASSOIFFE
    case "affame" => AFFAME
    case "appelle" => APPELER
    case "connaitre"=> CONNAITRE
    case "commander"=> COMMANDER
    case "moi" => MOI
    case "solde" => SOLDE
    case "combien" => COMBIEN
    case "coute" => COUTER
    case "quel" => QUEL
    case "maison" => MARQUE
    case "cailler" => MARQUE
    case "farmer" => MARQUE
    case "boxer" => MARQUE
    case "wittekop" => MARQUE
    case "punkipa" => MARQUE
    case "jackhammer" => MARQUE
    case "tenebreuse"  => MARQUE
    case "quel" => QUEL
    case "le" => USELESS
    case "de" => USELESS
    case "prix" => PRIX
    case p if p.startsWith("_") && p.length > 1 => PSEUDO // If the word starts with '_' and has more than one character it is a pseudonym.
    case n if n.forall(Character.isDigit) => NUM // If every character is a number, the word thus is a number.
    case _ => UNKNOWN
  }

  def tokenize(): Unit = {
    val words = input
      .trim()
      .replaceAll("[.,!?*]", " ") // Remove punctuation.
      .replaceAll(" +|[']", " ") // Remove multiple spaces and replace apostrophes by a space.
      .split(" ")
      .filterNot(_.isEmpty)

    // Get each word's occurence in the dictionary or check for the closest word if it is not contained in the dictionary.
    val fromDictionnary = words.map(w => dictionary.getOrElse(w, getClosestWordInDictionary(w)))

    tokens = fromDictionnary.map(t => (t, getTokenFromString(t)))
  }

  def nextToken(): (String, Token) = {
    currentTokenIndex += 1

    if (currentTokenIndex < tokens.length) tokens(currentTokenIndex)
    else ("EOL", EOL)
  }
}
