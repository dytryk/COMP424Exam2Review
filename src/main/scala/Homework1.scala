object Homework1 {
  def transcribe(s: String): String = for (c <- s) yield if (c == 'T') 'U' else c

  def reverseComplement(s: String): String = {
    for (c <- s.reverse) yield if (c == 'T') 'A' else if (c == 'A') 'T' else if (c == 'G') 'C' else 'G'
  }
}