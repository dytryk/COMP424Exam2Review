import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FindMotifSpec extends AnyFlatSpec with Matchers {

  "findMotif" should "handle an empty haystack" in {
    Homework2.findMotif("", "") should equal (Nil)
    Homework2.findMotif("", "AAT") should equal (Nil)
  }
  
  it should "find no occurrences of an empty motif" in {
    Homework2.findMotif("ATGC", "") should equal (Nil)
    Homework2.findMotif("A", "") should equal (Nil)
  }
  
  it should "handle single-character strings" in {
    Homework2.findMotif("A", "T") should equal (Nil)
    Homework2.findMotif("T", "A") should equal (Nil)
    Homework2.findMotif("T", "T") should equal (List(1))
  }
  
  it should "handle haystacks shorter than the motif" in {
    Homework2.findMotif("A", "TTTTTT") should equal (Nil)
    Homework2.findMotif("TAGCCGGCAT", "TAGCCGGCATA") should equal (Nil)
  }
  
  it should "handle motifs that do not occur" in {
    Homework2.findMotif("CATTTAGTTTAAC", "AAT") should equal (Nil)
    Homework2.findMotif("CAT", "CGT") should equal (Nil)
    Homework2.findMotif("AGGCTTAAGGCTCTCTTAGGCGATT", "AGGCTGAAGGCTCTCTTAGGCGATT") should equal (Nil)
    Homework2.findMotif("AGC", "TGC") should equal (Nil)
    Homework2.findMotif("AGC", "AGT") should equal (Nil)
  }
  
  it should "handle motifs that occur exactly once" in {
    Homework2.findMotif("AACCGGT", "T") should equal (List(7))
    Homework2.findMotif("AACCGG", "AC") should equal (List(2))
    Homework2.findMotif("ACGCGCGA", "CGA") should equal (List(6))
    Homework2.findMotif("CCCTCCCTCCCCTC", "CCCC") should equal (List(9))
  }

  it should "handle motifs that occur more than once" in {
    var res = Homework2.findMotif("AACCGGT", "A")
    res.toSet should equal (Set(1, 2))
    res.size should be (2)
    
    res = Homework2.findMotif("ACTGGACGCTACTA", "ACT")
    res.toSet should equal (Set(1, 11))
    res.size should be (2)

    res = Homework2.findMotif("ACTAGGACGCTAGTAATAGC", "TAG")
    res.toSet should equal (Set(3, 11, 17))
    res.size should be (3)
  }

  it should "handle motifs that occur in overlapping parts of the haystack" in {
    var res = Homework2.findMotif("AACAACAACAA", "AACAA")
    res.toSet should equal (Set(1, 4, 7))
    res.size should be (3)
    
    res = Homework2.findMotif("GGCAGGCAGGCAGGCA", "GCAGG")
    res.toSet should equal (Set(2, 6, 10))
    res.size should be (3)
  }
}


class FindMotifSpec2 extends AnyFlatSpec with Matchers {

  "findMotif" should "handle an empty haystack" in {
    LectureExcercises.findMotif("", "") should equal (Nil)
    LectureExcercises.findMotif("", "AAT") should equal (Nil)
  }

  it should "find no occurrences of an empty motif" in {
    LectureExcercises.findMotif("ATGC", "") should equal (Nil)
    LectureExcercises.findMotif("A", "") should equal (Nil)
  }

  it should "handle single-character strings" in {
    LectureExcercises.findMotif("A", "T") should equal (Nil)
    LectureExcercises.findMotif("T", "A") should equal (Nil)
    LectureExcercises.findMotif("T", "T") should equal (List(1))
  }

  it should "handle haystacks shorter than the motif" in {
    LectureExcercises.findMotif("A", "TTTTTT") should equal (Nil)
    LectureExcercises.findMotif("TAGCCGGCAT", "TAGCCGGCATA") should equal (Nil)
  }

  it should "handle motifs that do not occur" in {
    LectureExcercises.findMotif("CATTTAGTTTAAC", "AAT") should equal (Nil)
    LectureExcercises.findMotif("CAT", "CGT") should equal (Nil)
    LectureExcercises.findMotif("AGGCTTAAGGCTCTCTTAGGCGATT", "AGGCTGAAGGCTCTCTTAGGCGATT") should equal (Nil)
    LectureExcercises.findMotif("AGC", "TGC") should equal (Nil)
    LectureExcercises.findMotif("AGC", "AGT") should equal (Nil)
  }

  it should "handle motifs that occur exactly once" in {
    LectureExcercises.findMotif("AACCGGT", "T") should equal (List(7))
    LectureExcercises.findMotif("AACCGG", "AC") should equal (List(2))
    LectureExcercises.findMotif("ACGCGCGA", "CGA") should equal (List(6))
    LectureExcercises.findMotif("CCCTCCCTCCCCTC", "CCCC") should equal (List(9))
  }

  it should "handle motifs that occur more than once" in {
    var res = LectureExcercises.findMotif("AACCGGT", "A")
    res.toSet should equal (Set(1, 2))
    res.size should be (2)

    res = LectureExcercises.findMotif("ACTGGACGCTACTA", "ACT")
    res.toSet should equal (Set(1, 11))
    res.size should be (2)

    res = LectureExcercises.findMotif("ACTAGGACGCTAGTAATAGC", "TAG")
    res.toSet should equal (Set(3, 11, 17))
    res.size should be (3)
  }

  it should "handle motifs that occur in overlapping parts of the haystack" in {
    var res = LectureExcercises.findMotif("AACAACAACAA", "AACAA")
    res.toSet should equal (Set(1, 4, 7))
    res.size should be (3)

    res = LectureExcercises.findMotif("GGCAGGCAGGCAGGCA", "GCAGG")
    res.toSet should equal (Set(2, 6, 10))
    res.size should be (3)
  }
}