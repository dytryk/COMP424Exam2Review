import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountSourcesSpec extends AnyFlatSpec with Matchers {

  "countSources" should "return 6 for F" in {
    Homework4.countSources("F") should equal (6)
  }
  it should "count for Y" in {
    Homework4.countSources("Y") should equal (6)
  }
  it should "count for S" in {
    Homework4.countSources("S") should equal (18)
  }
  it should "count for L" in {
    Homework4.countSources("L") should equal (18)
  }
  it should "count for PHQ" in {
    Homework4.countSources("PHQ") should equal (48)
  }
  it should "count for KIM" in {
    Homework4.countSources("KIM") should equal (18)
  }
  it should "count for VADER" in {
    Homework4.countSources("VADER") should equal (1152)
  }
  it should "count for ACDEFGHIKLMNPQRSTVWY" in {
    Homework4.countSources("ACDEFGHIKLMNPQRSTVWY") should equal (215872)
  }
  it should "return the number of possible RNA sources, modulo 1000000 for a long amino acid sequence" in {
    val input = "ACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWYACDEFGHIKLMNPQRSTVWY"
    Homework4.countSources(input) should equal (799872)
  }

}