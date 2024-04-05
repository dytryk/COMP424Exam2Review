import scala.collection.mutable

object Homework2 {
  def countNucleotides(s: String) : Map[Char, Int] = {
    val countNucs = mutable.Map('A' -> 0, 'C' -> 0, 'G'-> 0, 'T' -> 0)
    for (c <- s) yield countNucs(c) += 1
    countNucs.toMap
  }

  def pointMutations(s1: String, s2: String) : Int = {
    if (s1.nonEmpty && (s1.charAt(0) != s2.charAt(0))) 1 + (if(s1.length > 1) then pointMutations(s1.substring(1), s2.substring(1)) else 0)
    else 0 + (if(s1.length > 1) then pointMutations(s1.substring(1), s2.substring(1)) else 0)
  }

  def findMotif(s1: String, s2: String) : List[Int] = {
    if (s1.equals("") || s2.equals("") || s2.length > s1.length) then return List()
    (for i <- 0 until 1+s1.length-s2.length yield if s1.substring(i, i+s2.length) == s2 then i+1 else -1).filter(_ >= 0).toList
  }
}