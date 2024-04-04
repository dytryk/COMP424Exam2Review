object OverlapFunction {
  def apply(n: Int): (String, String) => Boolean = (s1: String, s2: String) => s1.substring(s1.length()-n).equals(s2.substring(0, n)) && !s1.equals(s2)
}