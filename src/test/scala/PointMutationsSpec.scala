import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PointMutationsSpec extends AnyFlatSpec with Matchers {

  "pointMutations" should "handle the empty string" in {
    Homework2.pointMutations("", "") should be (0)
  }
  
  it should "handle single-character strings" in {
    Homework2.pointMutations("A", "T") should be (1)
    Homework2.pointMutations("T", "A") should be (1)
    Homework2.pointMutations("T", "T") should be (0)
  }
  
  it should "handle strings differing at one point" in {
    Homework2.pointMutations("CAT", "AAT") should be (1)
    Homework2.pointMutations("CAT", "CGT") should be (1)
    Homework2.pointMutations("CAT", "CAA") should be (1)
    Homework2.pointMutations("AGGCTTAAGGCTCTCTTAGGCGATT", "AGGCTGAAGGCTCTCTTAGGCGATT") should be (1)
  }
  
  it should "handle strings with multiple mutations" in {
    Homework2.pointMutations("CAT", "GGG") should be (3)
    Homework2.pointMutations("CAT", "CGG") should be (2)
    Homework2.pointMutations("CAT", "AAA") should be (2)
    Homework2.pointMutations("AGGCTTAAGGCTCTCTTAGGCGATT", "CCGCTGAAGGCTCTCTTAGGCGACC") should be (5)
    Homework2.pointMutations("CAT", "TAC") should be (2)
  }
}