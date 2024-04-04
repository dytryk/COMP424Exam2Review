import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckAnswersSpec extends AnyFlatSpec with Matchers {

  "checkAnswers" should "return true if all answers match" in {
     Homework6.checkAnswers("hello", 5, List( {_.length}, (x) => 5 ) ) should equal (true)
     Homework6.checkAnswers("nope", 4, List( {_.length} ) ) should equal (true)
  }
  
  it should "return false if at least one answer does not match" in {
    Homework6.checkAnswers("bye", 3, List( {_.length}, (x) => 5 ) ) should equal (false)
    Homework6.checkAnswers("abcde", 10, List( {_.indexOf("abc")}, (x) => 10 ) ) should equal (false)
    Homework6.checkAnswers("x", 10, List( (y) => 4 ) ) should equal (false)
    Homework6.checkAnswers("x", 10, List( (y) => 4, (z) => 6 ) ) should equal (false)
  }
}

