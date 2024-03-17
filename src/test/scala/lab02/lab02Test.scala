package lab02

import org.junit.*
import org.junit.Assert.*

class lab02Test:
  import lab02.*
  import lab02.Shape.*

  val b = 3
  val h = 5
  val r = 6.5
  val s = 2.5

  @Test def testPerimeter() =
    val delta = 0.001
    assertEquals(16.0, perimeter(Rectangle(b, h)), delta)
    assertEquals(40.84, perimeter(Circle(r)), delta)
    assertEquals(10.0, perimeter(Square(s)), delta)

  @Test def testScale() =
    assertEquals(Rectangle(9.0, 15.0), scale(Rectangle(b, h), 3))
    assertEquals(Circle(13.0), scale(Circle(r), 2))
    assertEquals(Square(7.5), scale(Square(s), 3))

  @Test def testNegGeneric() =
    assertFalse(negGeneric((x: Int) => x == 1)(1))  
