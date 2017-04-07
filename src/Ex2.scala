object Ex2 {

  def ex1() = {
    // Answer
    def fib(n: Int): Int = {
      @annotation.tailrec
      def loop(index: Int, prev: Int, result: Int): Int = {
        if (index == n) prev
        else loop(index + 1, result, prev + result)
      }
      loop(0, 0, 1)
    }

    // Usage
    (0 to 10).foreach(
      i => println(s"""$i => ${fib(i)}""")
    )
  }

  def ex2() = {
    // Answer
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def walk(array: Array[A]): Boolean = {
        if (array.length <= 1) true
        else ordered(array(0), array(1)) && walk(array.slice(1, array.length))
      }
      walk(as)
    }

    // Usage
    def log(array: Array[Int], ordered: (Int, Int) => Boolean): Unit = {
      val result: Boolean = isSorted(array, ordered)
      println(s"""${array.deep.toString()} => $result""")
    }

    val ordered: (Int, Int) => Boolean = (a: Int, b: Int) => {
      a < b
    }

    log(Array(), ordered)
    log((0 to 1).toArray, ordered)
    log((0 to 10).toArray, ordered)
    log(Array(1, 3, 1), ordered)
    log(Array(3, 1), ordered)

  }


  def ex3_4_5() = {
    // Answer
    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a, b) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C =
      a => f(g(a))

    // Usage
    def append(prefix: String, postfix: String): String = s"""$prefix $postfix"""

    println(curry(append)("prefix")("postfix"))
    println(uncurry(curry(append))("prefix", "postfix"))

    def intToString(n: Int): String = n.toString
    def listSize[A](list: List[A]): Int = list.length

    println(compose(intToString, listSize)(List()))
    println(compose(intToString, listSize)((1 to 10).toList))

    val format = curry(append)("The list size is")
    val sizeToString = compose(intToString, listSize)

    println(format(sizeToString(List())))
    println(format(sizeToString((1 to 10).toList)))
  }

  def main(args: Array[String]): Unit = {
    println("### 1 ")
    ex1()

    println("### 2 ")
    ex2()

    println("### 3, 4, 5 ")
    ex3_4_5()
  }

}
