object Ex3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  // Implement the function tail for removing the first element of a List.
  // Note that the function takes constant time.
  // What are different choices you could make in your implementation if the List is Nil? We’ll return to this question in the next chapter.
  def tail[A](list: List[A]) = {
    list match {
      case Cons(_, tail) => tail
      case _ => List()
    }
  }

  def setHead[A](newHead: A, list: List[A]) = {
    List(newHead, tail(list))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(dropped: Int, list: List[A]): List[A] = {
      if (dropped == n) list
      else loop(dropped + 1, tail(list))
    }
    loop(0, l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => 1 + b)

  @annotation.tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    list match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def inc(list: List[Int]): List[Int] = {
    def f(elt: Int, acc: List[Int]): List[Int] = {
      Cons(elt + 1, acc)
    }
    foldRight(list, List[Int]())(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    def f(list: List[A], element: A): List[A] = Cons(element, list)
    foldLeft(l, List[A]())(f)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((elt: A, acc: List[B]) => Cons(f(elt), acc))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def inner(elt: A, acc: List[A]): List[A] = {
      if (f(elt)) Cons(elt, acc)
      else acc
    }
    foldRight(as, List[A]())(inner)
  }

  def add(l1 : List[Int], l2 : List[Int]) : List[Int] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, add(tail1, tail2))
    }
  }

  def main(args: Array[String]): Unit = {
    def log[A](list: List[A]): Unit = {
      println(list)
    }

    //    log(tail(List()))
    //    log(tail(List(1, 2)))
    //    log(drop(List(1, 2), 0))
    //    log(drop(List(1, 2), 1))
    //    log(drop(List(1, 2), 2))
    //
    //    println(length(List()))
    //    println(length(List(1)))
    //    println(length(List(1, 2)))
    //    println(length(List(1, 2, 3)))
    //    println(length(List(1, 2, 3, 4)))


    def fLeft(a: String, b: Int): String = {
      s"""($a + $b)"""
    }

    def fRight(a: Int, b: String): String = {
      s"""($a + $b)"""
    }

    println("foldLeft :   " + foldLeft(List(1, 2, 3, 4), "0")(fLeft))
    println("foldRight:   " + foldRight(List(1, 2, 3, 4), "0")(fRight))
    println("reverse  :   " + reverse(List(1, 2, 3, 4)))
    println("increment:   " + inc(List(1, 2, 3, 4)))
    println("map      :   " + map(List(1, 2, 3, 4))(a => List(a)))
    println("filter   :   " + filter(List(1, 2, 3, 4))(a => a % 2 == 0))
    println("flatmap  :   " + flatMap(List(1, 2, 3))(i => List(i, i)))
    println("concat   :   " + concat(List(List(1), List(2), List(3))))
    println("add      :   " + add(List(1, 2, 3), List(3, 2, 1)))

  }
}
