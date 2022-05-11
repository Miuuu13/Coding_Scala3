@main
def main(): Unit = {
  println("Hello world!")
  val twoThrird = new Rational(2,3)
  val two = new Rational(2,1)
  val minusTwoThird = -twoThrird //prints the negated rational
  two.unary_- //-2
  twoThrird.reciprocal //3/2
  val four = new Rational(0,4)
  //four.reciprocal //thows wanted exception
  val fourFifth = new Rational(4, 5)
  fourFifth.reciprocal




}

/** Class for a Rational
 *
 * @param nominator the nominator of the Rational as Double
 * @param denominator the denominator of the Rational as Double
 * @throws IllegalArgumentException if passed denominator is zero or negative
 *
  */
class Rational(val n: Int, val d: Int) {
  //primary constructor - everything not capsuled in a function will be executed by instantiation
  if d <= 0 then
    throw new IllegalArgumentException("Negative denominator or denominator = 0!")

  /*
  //To shorten the rational, but seems to not work - import needed?
  private val g = gcd(n.abs, d.abs) * math.signum(d)
  val numerator: Int = n / g
  val denominator: Int = d / g
  */

  val numerator: Int = n // / g
  val denominator: Int = d // / g

  //to print the Rational when instantiated
  // 1a) Print only Integer value when denominator is 1
  if d == 1 then
    println(s"Integer value: $numerator")
  else
    println(s"Rational: $numerator / $denominator")

  // Negate the Rational

  /** To negate a Rational
   *
   * @param this implicit the rational that will be negated
   * @return new negated rational representing the negated rational
   *
   */
  def unary_- : Rational =
    new Rational(-n, d)

  // Reciprocal of the rational
  /** Create the reciprocal of a rational
   *
   * @param this implicit the rational of which the reciprocal is negated
   * @return new rational representing the reciprocal of the rational
   * @throws ArithmeticException if the denominator is 0
   *
   */
  def reciprocal: Rational =
    if n == 0 then
      throw new ArithmeticException("The numerator is 0!")
    else
      new Rational(d, n)


  // Substraction of rationals

  def -(other: Rational) =
    new Rational(
      numerator - other.numerator,
      denominator - other.denominator)

















}