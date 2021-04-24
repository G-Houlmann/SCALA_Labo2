package Utils

import scala.annotation.tailrec

/**
  * Contains the functions necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator {
  /**
    * Calculate the factorial of a given number.
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): BigInt = {
    /**
     * tail-recursive implementation
     * @param n the number to evaluate
     * @param acc accumulator
     * @return n!
     */
    @tailrec
    def facto_r(n: Int, acc: BigInt): BigInt = n match {
      case 0 => acc
      case _ => facto_r(n - 1, n * acc)
    }
    require(n >= 0)
    facto_r(n, 1)
  }

  /**
    * Calculate the combination of two given numbers.
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int =
    if (n < k) 0
    else (factorial(n) / (factorial(k) * factorial(n - k))).toInt

}
