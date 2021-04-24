package Utils

import Utils.Dictionary.dictionary

object SpellChecker {
  def stringDistance(s1: String, s2: String): Int = {
    // Memorize intermediary distances
    val memo = scala.collection.mutable.Map[(String, String), Int]()
    def min(a: Int, b: Int, c: Int) = Math.min(Math.min(a, b), c)

    def loop(s1: String, s2: String): Int = {
      memo.getOrElseUpdate((s1, s2), (s1, s2) match {
        case (_, "") => s1.length
        case ("", _) => s2.length
        case (x, y) =>
          val c1 = x.head
          val t1 = x.tail
          val c2 = y.head
          val t2 = y.tail

          val deletion = loop(t1, s2) + 1
          val insertion = loop(s1, t2) + 1
          val substitution = loop(t1, t2) + (if (c1 == c2) 0 else 1)
          min(deletion, insertion, substitution)
      })
    }

    loop(s1, s2)
  }

  def getClosestWordInDictionary(misspelledWord: String): String = {
    // If the word is a number we don't want to replace it so we just return it.
    if (misspelledWord.forall(Character.isDigit) || misspelledWord.startsWith("_")) {
      misspelledWord
    } else {
      // Build a list of tuples that contains the Levenshtein score for each of the dictionary's keys, according to the
      // given misspelled word, e.g. (("bonjour", 4), ("hello", 1), (...))
      val dictionaryDistanceScores = dictionary
        .map(x => (x._1, stringDistance(x._1, misspelledWord)))
        .toSeq

      // Sort the list by ascending score and return the top element's value (which is the closest word).
      dictionary.getOrElse(
        dictionaryDistanceScores.sortBy(_._1).minBy(_._2)._1, 
        throw new Error("Unable to find an occurence in the dictionary.")
      )
    }
  }
}
