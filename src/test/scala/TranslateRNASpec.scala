import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranslateRNASpec extends AnyFlatSpec with Matchers {
  "translateRna" should "translate RNA sequences to amino acid sequences" in {
//    Homework4.translateRna("UAG") should equal ("")
    Homework4.translateRna("GCCUAA") should equal ("A")
    Homework4.translateRna("CGAUAG") should equal ("R")
    Homework4.translateRna("UAUUUGAAGGGAUAUUGA") should equal ("YLKGY")
    Homework4.translateRna("UAUCGACGAUAA") should equal ("YRR")
  }
}

class TranslateRNASpec2 extends AnyFlatSpec with Matchers {
  "translateRna" should "translate RNA sequences to amino acid sequences" in {
    //    Homework4.translateRna("UAG") should equal ("")
    LectureExcercises.translateRna("GCCUAA") should equal ("A")
    LectureExcercises.translateRna("CGAUAG") should equal ("R")
    LectureExcercises.translateRna("UAUUUGAAGGGAUAUUGA") should equal ("YLKGY")
    LectureExcercises.translateRna("UAUCGACGAUAA") should equal ("YRR")
  }
}