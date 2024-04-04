import scala.Boolean

@main
def General () = {
  // Test cases
  val testCases = List(
    List(true, true, true), // Odd number of true values
    List(true, false, true), // Even number of true values
    List(false, false, false), // No true values
    List(true, true), // Odd number of true values
    List(false, false, true) // Even number of true values
  )

  // Run tests
  testCases.foreach(testCase => {
    val result = parityFoldBest(testCase)
    println(s"Input: $testCase, Output: $result")
  })

  val x = List(1, 2, 3)
  println(transform(x, _.toDouble))
  println(makeString)
}

//def parity(ls: List[Boolean]): Boolean = {
//  def count_trues(ls: List[Boolean], sumSoFar: Int): Int = {
//    if
//  }
//  ls.foldLeft(0)(if (_ == true) then false)
//}

// the good version:
def parityBasic(bits: List[Boolean]): Boolean = {
  var answer = false
  for (bit <- bits) {
    if (answer == false && bit == true){
      answer = true
    } else if (answer == true && bit == false) {
      answer = true
    } else {
      answer = false
    }
  }
  answer
}

def parityBasicCondensed(bits: List[Boolean]): Boolean = {
  var answer = false
  for (bit <- bits) {
    answer = (answer != bit) //XOR
  }
  answer
}

def parityFold(bits: List[Boolean]): Boolean =
  bits.foldLeft(false)((answer, bit) => answer != bit)

def parityFoldBest(bits: List[Boolean]): Boolean = bits.foldLeft(false)(_^_)

def transform[A,B](xs: List[A], f: A => B): List[B] = {
  for x <- xs yield f(x)
}

//def both[A](f1: A => Boolean, f2: A => Boolean): A => Boolean = {
//  def combined(x: A): Boolean = f1(x) && f2(x)
//  return combined
//}

// more succinct way:
def both[A](f1: A => Boolean, f2: A => Boolean): A => Boolean = {
  (x: A) => f1(x) && f2(x)
}

def longAll[A](fns: List[A => Boolean]): A => Boolean = {
  var answer = (x: A) => true
  for (f <- fns) {
    answer = both(answer, f)
  }
  answer
}

def all[A](fns: List[A => Boolean]): A => Boolean = fns.foldLeft((x: A) => true)(both)

def makeCounter(x: Int): List[Int] => Int = {
  (xs: List[Int]) => xs.count(_ > x)
}

//def localMax(f: Int => Double, minval: Int, maxval: Int): Double = {
//  (minval until maxval).map(_ => f(_)).toList().max
//}

def makeString[A](xs: Seq[A]): String = xs.foldLeft("")(_+_)

def makeString2[A](xs: Seq[A], delim: String = "") = xs.map(_.toString).reduce(_+delim+_)