package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
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
    require(c <= r && c >= 0 && r >= 0)
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    // Consumes character list one by one, returning 'state' tuple whose
    //   - first element is current parentheses nesting level, starting at 0
    //   - second element is remaining characters
    // If resulting nesting level is 0 then parentheses are balanced; otherwise not.
    @tailrec
    def scan(parenLevel: Int, chars: List[Char]): (Int, List[Char]) = chars match {
      case Nil => (parenLevel, Nil)
      case '(' :: tail => scan(parenLevel + 1, tail)
      case ')' :: tail => if (parenLevel > 0) scan(parenLevel - 1, tail) else (-1, Nil)
      case _ :: tail => scan(parenLevel, tail)
    }

    val (parenLevel, _) = scan(0, chars)
    parenLevel == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = money match {
    case 0 => 1
    case money if money < 0 => 0
    case _ => coins match {
      case Nil => 0
      case coin :: coinsTail => countChange(money - coin, coins) + countChange(money, coinsTail) 
    }
  }
}
