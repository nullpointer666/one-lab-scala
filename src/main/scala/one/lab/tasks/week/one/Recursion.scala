package one.lab.tasks.week.one

object Recursion {
  def printNTimes(n: Int, value: String): Unit = ???

  def gcd(a: Long, b: Long): Long = ???

  def nthFibonacciNumber(n: Int): Int = if (n <= 0) 0 else if (n <= 2) 1 else nthFibonacciNumber(n - 1) + nthFibonacciNumber(n - 2)

  def tailRecursiveFibonacciNumber(n: Int): Int = ???
}