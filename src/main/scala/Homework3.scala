object Homework3 {
  def profile(seq: Seq[String]): Seq[Map[Char, Int]] = {
    val inverseSeq: Seq[String] = seq.transpose.map(_.mkString)
    for (string <- inverseSeq) yield Map('A' -> string.count(_ == 'A'), 'C' -> string.count(_ == 'C'), 'G' -> string.count(_ == 'G'), 'T' -> string.count(_ == 'T')).filter { case (_, count) => count != 0 }
  }

  def consensus(seq: Seq[String]): String = {
    (for (map <- profile(seq)) yield map.maxBy(_._2)._1.toString).mkString
  }
}

class Fasta(val name: String, val dna: String) {
  def gcContent : Double = {
    dna.count(c => c == 'C' || c == 'G').toDouble / dna.length.toDouble
  }
}

object Fasta {
  def parseFastas(s: String): List[Fasta] = {
    val strings = s.split(">")
    val fastas = for { string <- strings
      if string.nonEmpty
      arrs = string.split("\n", 2)
      name = arrs(0)
      content = arrs(1).replace("\n", "")
    } yield new Fasta(name, content)
    fastas.toList
  }
}