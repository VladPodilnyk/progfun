package recfun

import math.abs
import scala.collection.immutable.HashMap


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
      def factorial(number: Int): Int = {
        def f(acc: Int, number: Int): Int = {
          if (number == 0) acc
          else f(acc * number, number - 1)
        }
        f(1, number)
      }
      factorial(r) / (factorial(c) * factorial(abs(r - c)))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isBalanced(stack: List[Char], seq: List[Char]): Boolean = {
        if ((seq.isEmpty && !stack.isEmpty) ||
            (!seq.isEmpty && seq.head == ')' && stack.isEmpty)) false
        else if (seq.isEmpty && stack.isEmpty) true
        else if (seq.head == '(') isBalanced('(' :: stack, seq.tail)
        else if (seq.head == ')' && !stack.isEmpty) isBalanced(stack.tail, seq.tail)
        else isBalanced(stack, seq.tail)
      }
      isBalanced(List(), chars)
    }
  
  /**
   * Exercise 3
   * TODO: return to this after a while and find a way to 
   * add memoization
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }