object Homework6 {
  def checkAnswers(s: String, correctAnswer: Int, fns: List[String => Int]): Boolean = {
    (for f <- fns yield f(s)).count(_ == correctAnswer) == fns.length
  }

  def makeClipper(f: Double => Double, upperLimit: Double): Double => Double = {
    (g: Double) => if(f(g) < upperLimit) f(g) else upperLimit
  }
}