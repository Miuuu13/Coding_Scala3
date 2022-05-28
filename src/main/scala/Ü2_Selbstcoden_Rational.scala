
//Code kann so in main kopiert werden und läuft dann
/*
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
  val res1 = fourFifth - twoThrird
  val res2 = two - 1 //does not work cause denominator negative
  val res3 = fourFifth - 1 //rundet falsch! hier kommt -1 raus
  val res4 = fourFifth.*(twoThrird)
  val res5 = fourFifth./(twoThrird)
  val res6 = fourFifth.min(twoThrird)
  println(res6)
  println(fourFifth.toString1)





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

  //Accept Int as parameter, use alternative constructor
  def this(n: Int) = this(n, 1)
  //das war von mir falsch: def this(n) = new Rational(n, 1)
  //man beschreibt hier praktisch die Eigenreferenz!


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

  // We want to substract also Ints from a Rational -> "Überladen" von -

  def -(other:Int) =
    new Rational(
      numerator - other * denominator, 1
      //denominator + (-(other * numerator))
    )

  /*  def *(other:Rational) =
      new Rational(
        numerator * other.denominator + other.numerator * denominator,
        other.numerator * denominator + numerator * other.denominator,
      )*/

  //multiply two rationals
  def *(other:Rational) =
    new Rational(
      numerator * other.numerator,
      other.denominator * denominator,
    )
  //divide two rationals
  //throw ArithmeticException if result is 0
  def /(other:Rational) =
    if (numerator * other.denominator) == 0 then
      throw new ArithmeticException("Result of Division is Zero")

    new Rational(
      numerator * other.denominator,
      other.numerator * denominator,
    )

  //return minimum

  def min(other: Rational) : Boolean =
    (numerator * other.denominator) < (denominator * other.numerator)
  //should return the min!?

  def toString1: String = s"$numerator / $denominator"
  //siehe Odersky S96

}


//---------------------------------------------------

//The following code is retyped from the ZT3 where an example solution is presented

package week02.rational.immutable

/** Represents a rational number
 *
 * @param n the passed numerator (might be reduced)
 * @param d the passed denominator (might be reduced)
 *
 * @throws IllegalArgumentException if the provided denominator is zero
 */
class Rational(n: Int, d: Int) {

  if d == 0 then
    throw new IllegalArgumentException("The provided denominator is zero")

  private val g = gcd(n.abs, d.abs) * math.signum(d)
  val numerator = n / g
  val denominator = d / g
  // println(s"Created $numerator/$denominator")

  /** Alternative constructor for creating a Rational from an Integer */
  def this(n: Int) = this(n, 1)

  /** Adds a Rational to this Rational
   *
   * @param that the Rational to add to this Rational
   * @return a new Rational representing the result of the addition
   */
  def +(that: Rational): Rational = {
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator
    )
  }

  /** Adds an Int to this Rational
   *
   * @param that the Int to add to this Rational
   * @return a new Rational representing the result of the addition
   */
  def +(that: Int): Rational = {
    new Rational(numerator + that * denominator, denominator)
  }

  /** Negates the this Rational
   * @return a new Rational representing the negated this Rational
   */
  def unary_- : Rational = new Rational(- numerator, denominator)
  //Klammern kann man weglassen. new bräuchte man gar nicht.

  /** Builds the recprocial of the this Rational
   *
   * @return a new Rational representing the recprocial this Rational
   */
  def reciprocal : Rational = {
    if (numerator == 0){
      throw new ArithmeticException("Trying to compute the reciprocal of zero.")
    } else {
      new Rational(denominator, numerator)
    }
  }

  /** Subtracts the that Rational from the this Rational
   *
   * @param that the Rational to substract from the this Rational
   * @return a new Rational representing the result of the substraction
   */
  def -(that: Rational): Rational = {
    new Rational(
      numerator * that.denominator - that.numerator * denominator,
      denominator * that.denominator)
  }
  //Man kann den - Operator aber auch auf existierende Methoden + und unary_- deligieren:
  //alternativ:

  def -(that: Rational): Rational = this + (-that)

  // ist sicherer aber nicht unbedingt performanter, da 2 Methoden gerufen werden müssen

  /** Subtracts an Int from the this Rational
   *
   * @param that the Int to substract from the this Rational
   * @return a new Rational representing the result of the substraction
   */
  def -(that: Int): Rational = {
    new Rational(numerator - that * denominator, denominator)
  }

  /** Multiplies the that Rational with the this Rational
   *
   * @param that the Rational to multiply with the this Rational
   * @return a new Rational representing the result of the multiplication
   */
  def *(that: Rational): Rational = {
    new Rational(
      numerator * that.numerator,
      denominator * that.denominator
    )
  }

  /** Divides the that Rational from the this Rational
   *
   * @param that the Rational to divide from the this Rational
   * @return a new Rational representing the result of the divison
   */
  def /(that: Rational): Rational = {
    if (that.numerator == 0) {
      throw new ArithmeticException("The numerator is 0, therfore no division is possible.")
    } else {
      new Rational(
        numerator * that.denominator,
        denominator * that.numerator)}
  }

  //Alternatif: this * that.reciprocal (an reciprocal-Methode deligieren) möglich!

  /** Returns true if that Rational is less than this Rational
   *
   * @param that the Rational for which it is tested whether it is less than this Rational
   */
  def lessThan(that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  /** Returns the larger Rational of this and that
   *
   * @param that the Rational to compare with this Rational
   */
  def max(that: Rational): Rational = {
    if lessThan(that) then that else this
  }

  /** Returns the smaller Rational of this and that
   *
   * @param that the Rational to compare with this Rational
   */
  def min(that: Rational): Rational = {
    if lessThan(that) then this else that
  }

  /** Prints a textual representation of this Rational
   * Delegieren an toString und die textuelle Repräsentation printen.*/
  def print(): Unit = {
    println(toString) //es ginge auch: println(this)
  }
  /** Override toString to get textual representation (no print on console!)
   * Ist eine Stringrepräsentation, die weiter verwendet werden kann.*/
  override def toString: String = {
    if (numerator == 0) {
      0.toString
    } else if (denominator == 1) {
      numerator.toString
    }else {
      s"$numerator/$denominator"
    }
  }

  /** Returns the gcd of two passed Ints */
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) then a else gcd(b, a % b)
  }
}

/** Extension for class "Int" to support the + operator for the argument "Rational" */
extension (i: Int) {
  /** Adds a Rational to this Int
   *
   * @param that the Rational to add
   * @return a new Rational containing the result of the addition
   */
  def +(that: Rational): Rational = that + i

  /** Subtracts a Rational from this Int
   *
   * @param that the Rational to substract
   * @return a new Rational containing the result of the substraction
   */
  def -(that: Rational): Rational = - that + i
}

*/