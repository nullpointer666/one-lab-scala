package one.lab.tasks.week.three

import scala.annotation.tailrec

object Collections {

  // duplicateNTimes(3, List(1,2,3)) == List(1,1,1,2,2,2,3,3,3)
  // duplicateNTimes(3, List()) == List()
  def duplicateNTimes[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def duplicateNTimesUtil(elem: A, cnt: Int = n, accumulator: List[A] = List[A]()): List[A] =
      if (cnt > 0) duplicateNTimesUtil(elem, cnt - 1, accumulator ++ List(elem))
      else accumulator

    @tailrec
    def duplicateEveryElement(accumulator: List[A] = List(), temp: List[A] = list): List[A] = temp match {
      case Nil => accumulator
      case elem :: rest => duplicateEveryElement(accumulator ++ duplicateNTimesUtil(elem), rest)
    }

    duplicateEveryElement()
  }

  // splitAtK(4, List(1,2,3,4,5,6,7,8,9)) == (List(1,2,3,4), List(5,6,7,8,9))
  // splitAtK(0, List(1,2,3)) == (List(), List(1,2,3))
  def splitAtK[A](k: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def splitAtKUtil(cnt: Int = k, accumulator: List[A] = List(), temp: List[A] = list): (List[A], List[A]) = temp match {
      case elem :: rest => if (cnt > 0) splitAtKUtil(cnt - 1, accumulator ++ List(elem), rest) else (accumulator, List(elem) ++ rest)
      case Nil => throw new NoSuchElementException
    }

    splitAtKUtil()
  }

  // removeKthElement(5, List(1,2,3,4,5,6)) == (List(1,2,3,4,5), 6)
  // removeKthElement(2, List(1,2,3,4,5,6)) == (List(1,2,4,5,6), 2)
  // removeKthElement(-3, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  // removeKthElement(1000, List(1,2,3,4,5,6)) == IndexOutOfBoundException
  def removeKthElement[A](k: Int, list: List[A]): (List[A], A) = ???
}
