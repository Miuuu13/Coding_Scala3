@main
def main(): Unit = {
  println("Hello world!")
  val zero = new Vector3D(0, 0, 0)
  val e1 = new Vector3D(1, 0, 0)
  val e2 = new Vector3D(0, 1, 0)
  val e3 = new Vector3D(0, 0, 1)

  // to test the implemented methods
  zero.+(e1)
  zero.-(e1)
  e1.-()
  //test multiplicaltion Double * vector
  val three: Double = 3.00
  e1.*(three)
  //test multiplicaltion vector * Double
  val threeVector = new Vector3D(3)
  //dot product
  println("Dot product: ")
  e1.dot(threeVector)

  threeVector.*(e1)
  //cross product:
  val randomVector = new Vector3D(2,4,5)
  e1.cross(randomVector)
  //length:
  println(threeVector.length())

  e1.isOrthogonal(e2)







}
//import for square root:
import scala.math._
/** Class for a 3D vector
 *
 * @param x the x value as Double
 * @param y the y value as Double
 * @param z the z value as Double
 *
 */
class Vector3D(val x: Double, val y: Double, val z: Double) {
  //primary constructor - everything not capsuled in a function will be executed by instantiation
   println(s"3 dimensional vector: ($x, $y, $z)")

  //alternative Constructor: Want to accept Double values (transform them into a vector)
  def this(x: Double) = this(x,0, 0)

  /** Method to add a 3D vector to another 3D vector
   *
   * @return a new 3D vector as result of the addition
   */
  def +(other: Vector3D): Vector3D =
    new Vector3D(
      x + other.x,
      y + other.y,
      z + other.z)

  /** Method to subtract a 3D vector from another 3D vector
   *
   * @return a new 3D vector as result of the subtraction
   */
  def -(other: Vector3D): Vector3D =
    new Vector3D(
      x - other.x,
      y - other.y,
      z - other.z)

  /** Method to negate a 3D vector
   *
   * @return a new 3D vector as result of the negation
   */
  def -(): Vector3D = new Vector3D(-x, -y, -z)

  /** Method to multiply a 3D vector and a Double
   *
   * @return a new 3D vector as result of the multiplication
   */
  def *(that: Double): Vector3D =
    new Vector3D(
      x * that,
      y * that,
      z * that)

  /** Method to multiply a Double with a 3D vector
   *
   * @param that the vector that is multiplied
   * @return a new 3D vector as result of the multiplication
   */
  def *(that: Vector3D): Vector3D =
    new Vector3D(
      this.x * that.x,
      this.y * that.y,
      this.z * that.z)

  /** Calculates the dot product (Skalarprodukt) of two vectors
   *
   * @return the dot product as Double
   */
  def dot(that: Vector3D): Double =
      this.x * that.x +
      this.y * that.y +
      this.z * that.z
  ///??? Es wird ein Vektor statt ein Double ausgegeben, wsl wegen Alternativem Konstruktor

  /** Calculates the cross product (Kreuzprodukt) of two vectors
   *
   * @return the cross product as new vector
   */
  def cross(that: Vector3D) =
    new Vector3D(
      this.y * that.z - this.z * that.y,
      this.z * that.x - this.x * that.z,
      this.x * that.y - this.y * that.x)

  /** Calculates the eucledian length of a given vector
   *
   * @return the lenght of a vector as Double
   */
  def length(): Double =
    sqrt(x * x + y * y + z * z)

  //Method to get normalized vector
  def normalized(): Vector3D = ???

  /** Method to check if two vectors are parallel (cross product ==0!)
   *
   * @param this given vector
   * @param that provided vector
   * @return true if they are parallel, false otherwise
   */
  def isParallel(that: Vector3D): Boolean =
    this.cross(that) == 0
  ///??? seems to be invalid code but why?

  /**Method to check if two vectors are othogonal (dot product == 0!)
   *
   * @param this given vector
   * @param that provided vector
   * @return true if they are orthogonal, false otherwise
   */
  def isOrthogonal(that: Vector3D): Boolean =
    this.dot(that) == 0
  ///??? seems to be invalid code but why?


}


///Code mit dem aus dem ZT ableichen!

/*
* Tipps aus ZT zu Transformationsmethoden:

//1.)
// Potenzmenge mit for-Schleife :/

def PowerSetLoops(baseSet: Set[Int]): Set[Set[Int]] = {
  val accum = mutable.Set[Set[Int]](Set())
  for (elem <- baseSet) {
    val withElemAdded = mutable.Set[Set[Int]]()
    for (subset <- accum) {
      accum ++= withElemAdded
    }
    accum.toSet
  }
}

//Mit foldLeft für äußere Schleife und map für innere Schleife :)

def powerSetFold(baseSet: Set[Int]): Set[Set[Int]] = {
  baseSet.foldLeft(Set(Set()))((accum, element) => accum.map(_ + element))
}

2.) Parallelität

Nachteil von for-Schleife: Läuft immer nur auf einem Prozessor in fester Reihenfolge
Mach das mit dem ersten, das mit dem zweiten…
-> map "verteilt" : Hier wird praktisch nur gesagt was gemacht wird, aber nicht in welcher Reihenfolge!
Also praktisch parallelisiert und somit viel schneller als eine for-Schleife
(Parallel collections kommen später nochmal in der VL)

Nice to know: ApacheSpark treibt das nochmal auf die Spitze (verwendet auch .map usw um Code auf mehreren Rechnern laufen zu lassen)

58:28
3. Tipp: Man muss auf Reihenfolge achten:

5mal Code mit dem Ziel mindestens eine positive Zahl in der ArraySeq zu finden:
(Das Programm weiß nicht, dass wir nur 1sen hinzugefügt haben, d.h. jede Zahl erfüllt die Bedingung  größer Null zu sein)


@main def findPositive(): Unit = {
}

*
* */



/*
//Ziel: Gebe aus, ob sich mind. eine positive Zahl in der Seq befindet
//! Das Programm weiß nicht, dass jedes Element (nur Einsen) das erfüllt.
@main def findPositive(): Unit = {

  //Test-Seq anlegen mit lauter 1sen
  val testSeq = ArraySeq.fill[Int](100_000_000)(elem = 1)

  //Folgende 5 Varianten geben true aus, wenn eine positive Zahl vorhanden ist,
  // sind aber unterschiedlich schnell/ effizient:

  println(testSeq.exists(n => n > 0))
  // exists stoppt sofort, nachdem ein Element die Bedingung erfüllt

  println(testSeq.count(n => n > 0) > 0)
  // Zählt wie oft Bedingung erfüllt ist und prüft ob Anzahl größer Null


  println(testSeq.filter(n => n > 0).nonEmpty)
  // Filtert alle Elemente heraus, die die Bedingung erfüllen und prüft ob die Menge nicht leer ist

  println(testSeq.map(n => n > 0).reduce((x,y) => x || y))
  // Mapt erst nach Bedingung n>0 und reduziert dann

  println(!testSeq.forall(n => n <= 0))
  // Prüft danach ob alle Elemente nicht kleiner gleich Null sind
  // forall bricht ab sobald es einmal zutrifft (wie exists)


  // count, filter und map müssen immer bis zum Ende durchgehen!
  // -> Laufzeit u.U. schlechter
}

*/