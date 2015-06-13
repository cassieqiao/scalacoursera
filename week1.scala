package demo

/**
 * Created by Cassie on 6/13/2015.
 */
object hello {
  def main(args: Array[String]) = {

    println(pascal(4,6))

    println(balance("(sjsjdkf)))".toList))

    println(countChange(5,List(1,2)))
  }
  /*Exercise 1: Pascals Triangle*/

  def pascal(c:Int,r:Int):Int = {if (c==0 || c== r) 1 else pascal(c-1,r-1)+pascal(c,r-1)}

  /* Exercise 2: Parentheses Balancing  */
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail,open+1)
      else if (chars.head == ')') open>0 && balanced(chars.tail,open-1)
      else balanced(chars.tail,open)
    balanced(chars,0)
  }
  
  /* Exercise 3: Counting Change */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0) return 1
    else if (coins.isEmpty) return 0
    else countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
}
