import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MakeClipperSpec extends AnyFlatSpec with Matchers {

  val xs: List[Double] = List(1.0, 2.0, 3.0, 4.0, 5.0)

  "makeClipper" should "work on case one" in {
     val sqrMaxTen = Homework6.makeClipper( (x: Double) => x * x, 10.0 )
     xs.map(sqrMaxTen) should equal (List(1.0, 4.0, 9.0, 10.0, 10.0))
  }
  
  it should "work on case two" in {
     val doubleMaxSix = Homework6.makeClipper( (x: Double) => 2 * x, 6.0)
     xs.map(doubleMaxSix) should equal (List(2.0, 4.0, 6.0, 6.0, 6.0))
  }
}