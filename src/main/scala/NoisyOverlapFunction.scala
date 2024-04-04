object NoisyOverlapFunction {
  def apply(n: Int, pointMutationsAllowed: Int): (String, String) => Boolean = {
    (s1: String, s2: String) => !s1.equals(s2) && s1.length() >= n && s2.length >= n && pointMutations(s1.substring(s1.length - n), s2.substring(0, n)) <= pointMutationsAllowed
  }
}

def pointMutations(s1: String, s2: String): Int = {
  if (s1.nonEmpty && (s1.charAt(0) != s2.charAt(0))) 1 + (if (s1.length > 1) then pointMutations(s1.substring(1), s2.substring(1)) else 0)
  else 0 + (if (s1.length > 1) then pointMutations(s1.substring(1), s2.substring(1)) else 0)
}