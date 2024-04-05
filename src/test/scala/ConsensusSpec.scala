import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConsensusSpec extends AnyFlatSpec with Matchers {

  // for testing consensus and profile
  val dnaTestGroups: Seq[Seq[String]] = Seq(
    Seq( "T", "T", "T"),
    Seq( "T", "A", "G", "G" ),
    List("AGC", "AAT", "CAT"),
    List("AGGAA", "CTAAA", "CCCCA", "GGGGA"), // consensus should be CGGAA
    Seq("AATGCGTAC",
      "TATATCCGA",
      "AAAGAGGGA",
      "TTATAGAGG",
      "ACTCACGTA")
  )

  "consensus" should "work for one sequence" in {
    Homework3.consensus(Seq("A")) should equal("A")
    Homework3.consensus(Seq("ACG")) should equal("ACG")
  }

  it should "work for multiple length-one sequences" in {
    Homework3.consensus(dnaTestGroups.head) should equal("T")
    Homework3.consensus(dnaTestGroups(1)) should equal("G")
  }
  it should "work for case 2" in {
    Homework3.consensus(dnaTestGroups(2)) should equal("AAT")
  }

  it should "work for case 3" in {
    Homework3.consensus(dnaTestGroups(3)) should equal("CGGAA")
  }

  it should "work for case 4" in {
    Homework3.consensus(dnaTestGroups(4)) should equal("AATGAGGGA")
  }
}

class ConsensusSpec2 extends AnyFlatSpec with Matchers {

  // for testing consensus and profile
  val dnaTestGroups: Seq[Seq[String]] = Seq(
    Seq( "T", "T", "T"),
    Seq( "T", "A", "G", "G" ),
    List("AGC", "AAT", "CAT"),
    List("AGGAA", "CTAAA", "CCCCA", "GGGGA"), // consensus should be CGGAA
    Seq("AATGCGTAC",
      "TATATCCGA",
      "AAAGAGGGA",
      "TTATAGAGG",
      "ACTCACGTA")
  )

  "consensus" should "work for one sequence" in {
    LectureExcercises.consensus(Seq("A")) should equal("A")
    LectureExcercises.consensus(Seq("ACG")) should equal("ACG")
  }

  it should "work for multiple length-one sequences" in {
    LectureExcercises.consensus(dnaTestGroups.head) should equal("T")
    LectureExcercises.consensus(dnaTestGroups(1)) should equal("G")
  }
  it should "work for case 2" in {
    LectureExcercises.consensus(dnaTestGroups(2)) should equal("AAT")
  }

  it should "work for case 3" in {
    LectureExcercises.consensus(dnaTestGroups(3)) should equal("CGGAA")
  }

  it should "work for case 4" in {
    LectureExcercises.consensus(dnaTestGroups(4)) should equal("AATGAGGGA")
  }
}