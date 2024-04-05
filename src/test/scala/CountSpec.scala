import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountSpec extends AnyFlatSpec with Matchers {

  "countNucleotides" should "count single occurrences" in {
    Homework2.countNucleotides("ACGT") should equal (Map('A' -> 1, 'C' -> 1, 'G' -> 1, 'T' -> 1))
  }
  
  it should "account for nucleotides that are not present" in {
    Homework2.countNucleotides("AG") should equal (Map('A' -> 1, 'C' -> 0, 'G' -> 1, 'T' -> 0))
  }
  
  it should "handle the empty string" in {
    Homework2.countNucleotides("") should equal (Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0))
  }
  
  it should "count multiple occurrences" in {
    Homework2.countNucleotides("ACGTTAAGTGTCTCATG") should equal (Map('A' -> 4, 'C' -> 3, 'G' -> 4, 'T' -> 6))
  }
  
  it should "count multiple occurrences in long strings" in {
    Homework2.countNucleotides("ACGTTAACGTGTCTCATGAATGCCTTGTAGCTGCGAGGTATCGGGGTCTTCTAGCGAGGCTGAGGTCGATGATGCTGCTAGTACGTGCTGATGAGCTGCTCGTCGTAGTCGTACGTCGGCGCGCGGCGCGATCTGATGCTAGCTAGTGGGGCGTCTTAGCGAGTTTATATTATTATTCTCGAGTCTAGTCTGACTATTGCGTATCTATGC") should equal (Map('A' -> 36, 'C' -> 44, 'G' -> 65, 'T' -> 65))
  }
}

class CountSpec2 extends AnyFlatSpec with Matchers {

  "countNucleotides" should "count single occurrences" in {
    LectureExcercises.countNucleotides("ACGT") should equal (Map('A' -> 1, 'C' -> 1, 'G' -> 1, 'T' -> 1))
  }

  it should "account for nucleotides that are not present" in {
    LectureExcercises.countNucleotides("AG") should equal (Map('A' -> 1, 'C' -> 0, 'G' -> 1, 'T' -> 0))
  }

  it should "handle the empty string" in {
    LectureExcercises.countNucleotides("") should equal (Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0))
  }

  it should "count multiple occurrences" in {
    LectureExcercises.countNucleotides("ACGTTAAGTGTCTCATG") should equal (Map('A' -> 4, 'C' -> 3, 'G' -> 4, 'T' -> 6))
  }

  it should "count multiple occurrences in long strings" in {
    LectureExcercises.countNucleotides("ACGTTAACGTGTCTCATGAATGCCTTGTAGCTGCGAGGTATCGGGGTCTTCTAGCGAGGCTGAGGTCGATGATGCTGCTAGTACGTGCTGATGAGCTGCTCGTCGTAGTCGTACGTCGGCGCGCGGCGCGATCTGATGCTAGCTAGTGGGGCGTCTTAGCGAGTTTATATTATTATTCTCGAGTCTAGTCTGACTATTGCGTATCTATGC") should equal (Map('A' -> 36, 'C' -> 44, 'G' -> 65, 'T' -> 65))
  }
}