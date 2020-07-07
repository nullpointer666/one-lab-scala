package one.lab.tasks.week.two

import scala.annotation.tailrec

object Collections {
  // getLast(List(1 ,2, 3, 4)) -> 4
  // getLast(List())           -> java.util.NoSuchElementException
  @tailrec
  def getLast[A](list: List[A]): A = list match {
    case Nil => throw new java.util.NoSuchElementException()
    case elem :: Nil => elem
    case _ :: rest => getLast(rest)
  }

  // getLastOption(List(1 ,2, 3, 4)) -> Some(4)
  // getLastOption(List())           -> None
  @tailrec
  def getLastOption[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case elem :: Nil => Some(elem)
    case _ :: rest => getLastOption(rest)
  }

  // getPreLast(List(1 ,2, 3, 4)) -> 3
  // getPreLast(List(1))          -> java.util.NoSuchElementException
  // getPreLast(List())           -> java.util.NoSuchElementException
  def getPreLast[A](list: List[A]): A = list match {
    case Nil => throw new java.util.NoSuchElementException()
    case _ :: Nil => throw new java.util.NoSuchElementException()
    case first :: _ :: Nil => first
    case _ :: rest => getPreLast(rest)
  }

  // getPreLastOption(List(1 ,2, 3, 4)) -> Some(3)
  // getPreLastOption(List(1))          -> None
  // getPreLastOption(List())           -> None
  def getPreLast[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case _ :: Nil => None
    case first :: _ :: Nil => Some(first)
    case _ :: rest => getPreLast(rest)
  }

  // getNthElement(3, List(1 ,2, 3, 4)) -> 3
  // getNthElement(3, List(1))          -> java.lang.IndexOutOfBoundsException
  def getNthElement[A](n: Int, list: List[A]): A = {
    @tailrec
    def getNthElementUtil(count: Int, temp: List[A]): A = temp match {
      case Nil => throw new java.util.NoSuchElementException()
      case elem :: rest => if (count >= n) elem else getNthElementUtil(count + 1, rest)
    }

    if (n > list.size) throw new java.lang.IndexOutOfBoundsException()
    getNthElementUtil(1, list)
  }

  // getNthElementOption(3, List(1 ,2, 3, 4)) -> Some(3)
  // getNthElementOption(3, List(1))          -> None
  def getNthElementOption[A](n: Int, list: List[A]): Option[A] = {
    @tailrec
    def getNthElementUtil(count: Int, temp: List[A]): Option[A] = temp match {
      case Nil => None
      case elem :: rest => if (count >= n) Some(elem) else getNthElementUtil(count + 1, rest)
    }

    if (n > list.size) None
    getNthElementUtil(1, list)
  }

  // getLength(List(1,2,3)) -> 3
  // getLength(List())      -> 0
  def getLength[A](list: List[A]): Int = {
    @tailrec
    def getLengthUtil(count: Int, temp: List[A]): Int = temp match {
      case Nil => count
      case _ :: rest => getLengthUtil(count + 1, rest)
    }

    getLengthUtil(0, list)
  }

  // getReversedList(List(1,2,3)) -> List(3,2,1)
  def getReversedList[A](list: List[A]): List[A] = {
    @tailrec
    def getReversedListUtil[A](accumulator: List[A], temp: List[A]): List[A] = temp match {
      case Nil => accumulator
      case elem :: rest => getReversedListUtil(List(elem) ++ accumulator, rest)
    }

    getReversedListUtil(List(), list)
  }

  // duplicateEveryElement(List(1,2,3)) -> List(1,1,2,2,3,3)
  def duplicateEveryElement[A](list: List[A]): List[A] = {
    @tailrec
    def duplicateEveryElementUtil(accumulator: List[A], temp: List[A]): List[A] = temp match {
      case Nil => accumulator
      case elem :: rest => duplicateEveryElementUtil(accumulator ++ List(elem, elem), rest)
    }

    duplicateEveryElementUtil(List(), list)
  }
}