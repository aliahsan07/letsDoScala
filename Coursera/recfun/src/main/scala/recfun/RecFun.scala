package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println("Balance")
    val toTest = "())(".toList
    println(balance(toTest.toList))

    println("Counting Change")
    val money = 4
    val coins: List[Int] = List(1, 2)
    countChange(money, coins)



  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal (c, r-1)
  }

  /**
   * Exercise 2
   */

  def isBalance(chars: List[Char], test: List[Char]): Boolean = {
    if (chars.isEmpty) test.isEmpty else
    chars.head match {
      case ')' => if (test.isEmpty) false else isBalance(chars.tail, test.tail)
      case '(' => isBalance(chars.tail, '(' :: test)
      case _ => isBalance(chars.tail, test)
    }
  }

  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true else isBalance(chars, List[Char]())
  }

  /**
   * Exercise 3
   */


  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty)
      0
    else if (money == 0)
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    // think recursively
    // you are only doing two things.
    // 1. keep the current coin, find its combinations => money - coin
    // 2. remove the current coin, find combinations without the current coin => coins.tail

  }
}
