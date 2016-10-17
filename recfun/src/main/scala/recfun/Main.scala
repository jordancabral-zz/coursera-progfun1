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
    if (c == 0 ) 1
    else if (r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checkParenthesesBalance(openParentheses: Int , chars: List[Char]): Boolean = {
      if (chars.isEmpty) openParentheses match {
        case 0 => true
        case other => false
      }
      else chars.head match {
        case '(' => checkParenthesesBalance(openParentheses + 1 , chars.tail)
        case ')' => if (openParentheses > 0) checkParenthesesBalance(openParentheses - 1 , chars.tail) else false
        case other => checkParenthesesBalance(openParentheses , chars.tail)
      }
    }

    if (chars.isEmpty) true
    else checkParenthesesBalance(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
