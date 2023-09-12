package recfun

import scala.annotation.switch

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c < 0 || r < 0 then 0
    else if c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
  def difference(acc: Int, chars: List[Char]): Boolean =
    if chars.isEmpty then acc == 0
    else
      if chars.head == ')' && acc == 0 then return false
      val newAcc: Int =
        chars.head match
          case '(' => acc + 1
          case ')' => acc - 1
          case _ => acc
      difference(newAcc, chars.tail)

  difference(0, chars)
}


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money > 0 && coins.nonEmpty then
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else if money == 0 then 1
      else 0

