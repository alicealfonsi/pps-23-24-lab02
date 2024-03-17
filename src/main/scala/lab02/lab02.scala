package lab02

object lab02 extends App {
  /**
   * Task 1
  */
  println("Hello, Scala")

  /**
   * Task 2a
  */
  // a)
  // positive function as a val lambda
  val positiveVal: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"
  
  println(positiveVal(7)) // positive
  println(positiveVal(-7)) // negative

  // positive function with method syntax
  def positiveDef(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positiveDef(10)) // positive
  println(positiveDef(-10)) // negative

  // b)
  // neg function as a val lambda
  val negVal: (String => Boolean) => (String => Boolean) = pred => a => !pred(a)

  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negVal(empty)
  println(notEmpty("foo")) // true
  println(notEmpty("")) // false
  println(notEmpty("foo") && !notEmpty("")) // true

  // neg function with method syntax
  // def neg(pred: String => Boolean) : String => Boolean = (a: String) => !pred(a)
  def negDef(pred: String => Boolean) : String => Boolean = a => !pred(a)

  println(negDef(a => true)("ciao")) // false

  // c)
  // neg function for generic predicates
  // def neg[X](pred: X => Boolean) : X => Boolean = (a: X) => !pred(a)
  def negGeneric[X](pred: X => Boolean) : X => Boolean = a => !pred(a)

  println(negGeneric((x: Int) => x == 1)(1)) // false

  /**
   * Task 2b
  */
  // curried val
  val p1: Double => Double => Double => Boolean = x => y => z => (x <= y && y == z)

  println(p1(2)(5)(5)) // true

  // non-curried val
  val p2: (Double, Double, Double) => Boolean = (x, y, z) => (x <= y && y == z)
  
  println(p2(2, 5, 5)) // true

  // curried def
  def p3(x: Double)(y: Double)(z: Double): Boolean = x <= y && y == z

  println(p3(3)(4)(4)) // true

  // non-curried def
  def p4(x: Double, y: Double, z: Double): Boolean = x <= y && y == z

  println(p4(6, 2, 3)) // false

  // functional composition
  def compose(f: Int => Int, g: Int => Int): Int => Int = x => (f(g(x)))

  println(compose(_ - 1, _ * 2)(5)) // 9

  // generic version of compose
  def composeGeneric[X, Y, Z](f: Y => Z, g: X => Y): X => Z = x => (f(g(x)))

  println(composeGeneric((_: Double) / 3, (_: Double) * 2)(5.5)) // 3.666

  /**
   * Task 3
  */
  // greatest common divisor
  def gcd(a: Int, b: Int): Int =
    @annotation.tailrec
    def _gcd(a: Int, b: Int): Int = a match
      case 0 => b
      case a if b == 0 => a
      case _ => _gcd(b, a % b)
    _gcd(Math.abs(a), Math.abs(b))

  println(gcd(12, 8)) // 4
  println(gcd(5, 20)) // 5

  /**
   * Task 4
  */
  // geometric shapes
  enum Shape:
    case Rectangle(base: Double, height: Double)
    case Circle(radius: Double)
    case Square(side: Double)

  object Shape: // module with two methods for computing perimeter and scaling a shape, respectively
    
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(b, h) => (b + h) * 2
      case Circle(r) => 2 * java.lang.Math.PI * r
      case Square(s) => 4 * s
    
    def scale(shape: Shape, alpha: Double): Shape = shape match
      case Rectangle(b, h) => Rectangle(b * alpha, h * alpha)
      case Circle(r) => Circle(r * alpha)
      case Square(s) => Square(s * alpha)
    
  /**
   * Task 5
  */
  // map
  import task5.Optionals.*
  import Optional.*
  def map[A, B](opt: Optional[A])(f: A => B): Optional[B] = opt match
    case Maybe(value) => Maybe(f(value))
    case _ => Empty()
  
  println(map(Maybe(5))(_ > 2)) // Maybe(true)
  println(map(Empty())((_: Int) > 2)) // Empty()

  // filter
  def filter[A](opt: Optional[A])(pred: A => Boolean): Optional[A] = opt match
    case Maybe(value) if pred(value) => Maybe(value)
    case _ => Empty()
  
  println(filter(Maybe(5))(_ > 2)) // Maybe(5)
  println(filter(Maybe(5))(_ > 8)) // Empty()
  println(filter(Empty())((_: Int) > 2)) // Empty()
}
