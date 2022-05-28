object Uebung4_Rectangle_Collections {

  package week04.geometry.transformations

  import scala.collection.immutable.ArraySeq

  import week04.geometry.*


  def scaleAll(scaleFactor: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    rectangles.map(x => x * scaleFactor)


  def centerAroundOrigin(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    rectangles.map(r => r.offset(- r.xmin - r.width / 2, - r.ymin - r.height / 2))


  def filterAreaBetween(lowerBound: Double, upperBound: Double, rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
      rectangles.filter(r => (r.area >= lowerBound && r.area <= upperBound))


  def removeEmpty(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    rectangles.filter(r => r.area > 0)


  def hasSquare(rectangles: ArraySeq[Rectangle]): Boolean =
    rectangles.exists(r => r.isSquare)


  def allContainedIn(target: Rectangle, candidates: ArraySeq[Rectangle]): Boolean =
    rectangle.forall(r => r.contains(candidates))


  def findOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] =
    rectangle.reduceOption(r => r.contains(candidates)).getOrElse(0)


  def scaleToUnitArea(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    rectangles.filter(r => r.isEmpty())


  def totalCircumference(rectangles: ArraySeq[Rectangle]): Double =
    rectangles.foldLeft(0)((agg: Int, elem: Rectangle) => agg + elem.circumference)


  def overlap(rectangles: ArraySeq[Rectangle]): Option[Rectangle] =
    rectangles.reduceOption(r => r.overlap).getOrElse(0)


  def splitHorizontally(rectangles: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    rectangles.reduceOption(r => r.map(r = Rectangle(r.xmin, r.ymin, r.width / 2, r.height))).getOrElse(0)


  def allIntersections(targets: ArraySeq[Rectangle], candidates: ArraySeq[Rectangle]): ArraySeq[Rectangle] =
    targets.zip(candidates).getOrElse(0)


  def smallestAreaRectangle(rectangles: ArraySeq[Rectangle]): Option[Rectangle] =
    rectangles.reduceOption((x,y) => x.area > y.area).getOrElse(0)

  def smallestScaleFactorToContain(outer: Rectangle, inner: Rectangle, scaleFactors: ArraySeq[Double]): Option[Double] =



  def boundingBox(rectangles: ArraySeq[Rectangle]): Rectangle = ???


  def findSmallestOverlappingRectangle(target: Rectangle, candidates: ArraySeq[Rectangle]): Option[Rectangle] = ???


  def largestAreaWithCircumferenceAtMost(maxCircumference: Double, rectangles: ArraySeq[Rectangle]): Option[Double] = ???


  /** Returns the percentage of squares in a sequence of [[Rectangle]]s
   *
   *  @param rectangles the input [[Rectangle]]s
   */
  def squarePercentage(rectangles: ArraySeq[Rectangle]): Double = {
    if (rectangles.isEmpty) throw IllegalArgumentException("percentage of empty list")
    val squareCount = rectangles.count(r => r.isSquare)
    squareCount / rectangles.size * 100
  }


}
