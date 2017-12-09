package recfun

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
      (c, r) match {
        case (0, _) => 1
        case (c, r) if c > r + 1 => 0
        case (c, r) if c == r => 1
        case (c, r) =>
          pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def _balance(chars: List[Char], delta: Int): Int = {
        if (delta < 0) -1
        else
          chars match {
            case '(' :: tail => _balance(tail, delta + 1)
            case ')' :: tail => _balance(tail, delta - 1)
            case _ :: tail => _balance(tail, delta)
            case Nil => delta
          }
      }

      _balance(chars, 0) == 0
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      coins match {
        case Nil => 0
        case coin :: cons => {
          (money - coin) match {
            case 0 => return 1 + countChange(money, cons)
            case change if change < 0 => countChange(money, cons)
            case change if change > 0 => countChange(change, coins) + countChange(money, cons)
          }
        }
      }
    }
  }
