package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) 0
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
   def balance(chars: List[Char]): Boolean = {
    def difference(acc: Int, ch: List[Char]): Int = {
      val head = ch.head
      val tail = ch.tail
      val isLeft = head == '('
      val isRight = head == ')'
      val notEmptyTail = tail.nonEmpty
      if (acc == 1) -1
      else if (!isLeft && !isRight)
        if (notEmptyTail) difference(acc, tail) else acc
      else if (isLeft)
        if (notEmptyTail) difference(acc - 1, tail) else acc - 1
      else if (notEmptyTail) difference(acc + 1, tail) else acc + 1
    }

    difference(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money > 0 && !coins.isEmpty)
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    else if (money == 0) 1 else 0
  }
