import scala.annotation.tailrec

object Homework4 {
  def translateRna(rna: String): String = {
    (for (i <- 0 until rna.length) yield if (i == 0 || i % 3 == 0) { CodonTable.getAminoAcid(rna.substring(i, i+3))} else "").filter(_ != "").filter(_ != "Stop").mkString
  }
  
//  This is a variation of the above function that uses tail recursion instead of a for comprehension.  
//  @tailrec
//  def translateRna(rna: String, stringSoFar: List[String] = List()): String = rna match {
//    case "" => stringSoFar.filter(_ != "").filter(_ != "Stop").mkString
//    case _ => translateRna(rna.substring(3, rna.length), stringSoFar.appended(CodonTable.getAminoAcid(rna.substring(0, 3))))
//  }

  @tailrec
  def countSources(protein: String, numSoFar: Int = 1): Int = protein match {
    case "" => (numSoFar*3)%1000000
    case _ => countSources(protein.substring(1), (numSoFar * CodonTable.numPossCodons(protein.charAt(0).toString))%1000000)
  }
}