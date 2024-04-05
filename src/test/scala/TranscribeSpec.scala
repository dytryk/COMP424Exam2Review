import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranscribeSpec extends AnyFlatSpec with Matchers {

  "transcribe" should "handle the empty string" in {
    Homework1.transcribe("") should equal ("")
  }
  
  it should "transcribe T" in {
    Homework1.transcribe("T") should equal ("U")
  }
  
  it should "transcribe single-character strings" in {
    Homework1.transcribe("A") should equal ("A")
    Homework1.transcribe("C") should equal ("C")
    Homework1.transcribe("G") should equal ("G")
  }

  it should "transcribe several strings" in {
    Homework1.transcribe("ACGTACGT") should equal ("ACGUACGU")
    Homework1.transcribe("GCGAGATCTCAAAAGCCGCCG") should equal ("GCGAGAUCUCAAAAGCCGCCG")
  }
}

class TranscribeSpec2 extends AnyFlatSpec with Matchers {

  "transcribe" should "handle the empty string" in {
    LectureExcercises.transcribe("") should equal ("")
  }

  it should "transcribe T" in {
    LectureExcercises.transcribe("T") should equal ("U")
  }

  it should "transcribe single-character strings" in {
    LectureExcercises.transcribe("A") should equal ("A")
    LectureExcercises.transcribe("C") should equal ("C")
    LectureExcercises.transcribe("G") should equal ("G")
  }

  it should "transcribe several strings" in {
    LectureExcercises.transcribe("ACGTACGT") should equal ("ACGUACGU")
    LectureExcercises.transcribe("GCGAGATCTCAAAAGCCGCCG") should equal ("GCGAGAUCUCAAAAGCCGCCG")
  }
}
