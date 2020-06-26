package one.lab.tasks.week.one

import scala.annotation.tailrec

object Recursion {
  def printNTimes(n: Int, value: String): Unit = ???

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def nthFibonacciNumber(n: Int): Int =
    if (n <= 0) 0 else if (n <= 2) 1 else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    @tailrec
    def fibonacciUtil(accumulator: Int, previousValue: Int, counter: Int): Int =
      if (counter >= n) accumulator else fibonacciUtil(accumulator + previousValue, accumulator, counter + 1)

    if (n <= 0) 0 else fibonacciUtil(1, 1, 2)
  }
}