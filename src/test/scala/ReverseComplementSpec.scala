import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseComplementSpec extends AnyFlatSpec with Matchers {

  "reverseComplement" should "handle the empty string" in {
    Homework1.reverseComplement("") should equal ("")
  }
  
  it should "complement single-character strings" in {
    Homework1.reverseComplement("T") should equal ("A")
    Homework1.reverseComplement("A") should equal ("T")
    Homework1.reverseComplement("G") should equal ("C")
    Homework1.reverseComplement("C") should equal ("G")
  }
  
  it should "complement multi-character strings" in {
    Homework1.reverseComplement("CAT") should equal ("ATG")
    Homework1.reverseComplement("GGGGGGAAAAAAAAAATCGA") should equal ("TCGATTTTTTTTTTCCCCCC")
    Homework1.reverseComplement("TTTTT") should equal ("AAAAA")
  }
}

class ReverseComplementSpec2 extends AnyFlatSpec with Matchers {

  "reverseComplement" should "handle the empty string" in {
    LectureExcercises.reverseComplement("") should equal ("")
  }

  it should "complement single-character strings" in {
    LectureExcercises.reverseComplement("T") should equal ("A")
    LectureExcercises.reverseComplement("A") should equal ("T")
    LectureExcercises.reverseComplement("G") should equal ("C")
    LectureExcercises.reverseComplement("C") should equal ("G")
  }

  it should "complement multi-character strings" in {
    LectureExcercises.reverseComplement("CAT") should equal ("ATG")
    LectureExcercises.reverseComplement("GGGGGGAAAAAAAAAATCGA") should equal ("TCGATTTTTTTTTTCCCCCC")
    LectureExcercises.reverseComplement("TTTTT") should equal ("AAAAA")
  }
}