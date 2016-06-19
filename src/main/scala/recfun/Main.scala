package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0)
      1
    else if (r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def isBalanced(chars: List[Char], missingParenthesis: Int): Boolean = {
      if (missingParenthesis < 0)
        false
      else if (chars.isEmpty && missingParenthesis == 0)
        true
      else if (chars.head == ')')
        isBalanced(chars.tail, missingParenthesis - 1)
      else if (chars.head == '(')
        isBalanced(chars.tail, missingParenthesis + 1)
      else
        isBalanced(chars.tail, missingParenthesis)
    }
    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int], countSoFar: Int): Int = {
      if (coins.isEmpty) {
        countSoFar
      } else {
        val firstMoney: Int = coins.head
        val rest: Int = money - firstMoney
        if (rest == 0) {
          countChangeIter(money, coins.tail, countSoFar + 1)
        } else if (rest > 0) {
          countChangeIter(money - firstMoney, coins, countSoFar) + countChangeIter(money, coins.tail, countSoFar)
        } else {
          countSoFar
        }
      }
    }
    countChangeIter(money, coins.sorted, 0)
  }
}
