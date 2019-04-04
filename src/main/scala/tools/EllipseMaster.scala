package tools

import scalismo.geometry
import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh
import tools.ellipseCalculationMethod.ellipseCalculationMethod

object  EllipseMaster {

  def calculateCircumference(mesh: TriangleMesh[_3D], ellipsePost: Point[_3D], ellipseAnt: Point[_3D], ellipseSide: Point[_3D], method: ellipseCalculationMethod): Double = {

    // Find true points on mesh
    val truePost = mesh.pointSet.findClosestPoint(ellipsePost).point
    val trueAnt = mesh.pointSet.findClosestPoint(ellipseAnt).point
    val trueSide = mesh.pointSet.findClosestPoint(ellipseSide).point

    // Calculate a and b for ellipse
    // Assume right ellipse (a > b)
    // For a find distance between the side and the midpoint between ant and post
    val midpoint = (trueAnt.toVector + truePost.toVector)/2
    val a = distance(trueSide, midpoint.toPoint)
    val b = distance(truePost, trueAnt)/2
    val n = 100
// TODO: Add AGM algorithm
    method match {
      case tools.ellipseCalculationMethod.NUMERICAL =>
        numerical(a, b, n)
      case tools.ellipseCalculationMethod.INTEGRAL =>
        integral(a, b, n)
      case tools.ellipseCalculationMethod.ANONYMOUS =>
        anonymous(a, b)
      case tools.ellipseCalculationMethod.HUDSON =>
        hudson(a, b)
      case tools.ellipseCalculationMethod.RAMANUJAN1 =>
        ramanujan1(a, b)
      case tools.ellipseCalculationMethod.RAMANUJAN2 =>
        ramanujan2(a, b)
      case tools.ellipseCalculationMethod.HOLDERHIGH =>
        holderHigh(a, b)
      case tools.ellipseCalculationMethod.HOLDERLOW =>
        holderLow(a, b)
      case tools.ellipseCalculationMethod.CANTRELL =>
        cantrell(a, b)
      case tools.ellipseCalculationMethod.UNKNOWN =>
        -1.0
    }
  }

  /* Approximations from http://paulbourke.net/geometry/ellipsecirc/
     Correction to Bessel approximation applied
        See https://en.wikipedia.org/wiki/Ellipse#Circumference under Ivory and Bessel derivation
  */
  // Numerical approximation of circumference
  def numerical(a: Double, b: Double, n: Int): Double = {
    var len = 0.0
    var xLast = 0.0
    var yLast = 0.0
    var major = 0.0
    var minor = 0.0

    // Check what is major and what is minor for axis
    if (a >= b){
      major = a
      minor = b
    }
    else{
      major = b
      minor = a
    }

    for (i <- 0 to n){
      val theta = 0.5 * math.Pi * i/n.toDouble
      val x = major * math.cos(theta)
      val y = minor * math.sin(theta)

      if (i > 0){
        val xDiff = x - xLast
        val yDiff = y - yLast
        len += math.sqrt(xDiff*xDiff + yDiff*yDiff)
      }

      xLast = x
      yLast = y

    }
    len * 4
  }

  // Integral approximation of circumference
  def integral(a: Double, b: Double, n: Int): Double = {
    var len = 0.0
    var major = 0.0
    var minor = 0.0

    // Check what is major and what is minor for axis
    if (a >= b){
      major = a
      minor = b
    }
    else{
      major = b
      minor = a
    }

    val dt = 0.5 * math.Pi/n
    val e2 = 1.0 - ((minor*minor)/(major*major)) // Eccentricity squared

    for (i <- 0 until n){
      val theta = (0.5 * math.Pi * (i + 0.5))/n.toDouble
      val temp = 1 - e2 * math.sin(theta) * math.sin(theta)
      val add = math.sqrt(temp) * dt
      len += add
    }
    len * 4 * major
  }

  // Circle approximation. When a == b
  def circle(a: Double): Double = {
    2 * a * math.Pi
  }
  // Anonymous approximation
  def anonymous (a: Double, b: Double): Double = {
    math.Pi * math.sqrt(2 * (a*a + b*b) - 0.5 * (a - b) * (a - b))
  }

  // Hudson approximation
  def hudson (a: Double, b: Double): Double = {
    val h = math.pow(a - b, 2)/math.pow(a + b, 2)
    0.25 * math.Pi * (a + b) * (3 * (1 + h/4) + 1/(1 - h/4))
  }

  // Ramanujan 1 approximation
  def ramanujan1 (a: Double, b: Double): Double = {
    math.Pi * (3 * (a + b) - math.sqrt((a + 3 * b) * (3 * a + b)))
  }

  // Ramanujan 2 approximation
  def ramanujan2 (a: Double, b: Double): Double = {
    val h = math.pow(a - b, 2)/math.pow(a + b, 2)
    math.Pi * (a + b) * (1 + 3 * h/(10 + math.sqrt(4 - 3 * h)))
  }

  // Holder mean approximation for higher eccentricities
  def holderHigh (a: Double, b: Double): Double = {
    val s = math.log(2)/math.log(math.Pi / 2)
    4 * math.pow(math.pow(a, s) + math.pow(b, s), 1/s)
  }

  // Holder mean approximation for lower eccentricities
  def holderLow (a: Double, b: Double): Double = {
    val s = 1.5
    2 * math.Pi * math.pow(math.pow(a, s)/2 + math.pow(b, s)/2, 1/s)
  }

  // Cantrell mean approximation for lower eccentricities
  def cantrell (a: Double, b: Double): Double = {
    var major = 0.0
    var minor = 0.0

    // Check what is major and what is minor for axis
    if (a >= b){
      major = a
      minor = b
    }
    else{
      major = b
      minor = a
    }

    val e = math.sqrt(1.0 - ((minor*minor)/(major*major)))
    var s = 0.0

    if (e < 0.99){
      s = (3 * math.Pi - 8)/(8 - 2* math.Pi)
    }
    else{
      s = math.log(2.0)/math.log(2.0/(4 -  math.Pi))
    }

    4 * (major + minor) - (2 * (4 - math.Pi) * major * minor)/math.pow(math.pow(major, s)/2.0 + math.pow(minor, s)/2.0, 1/s)
  }

  private def distance(a: Point[_3D], b: Point[_3D]): Double = {
    math.sqrt(math.pow(a.x - b.x,2) + math.pow(a.y - b.y,2) + math.pow(a.z - b.z,2))
  }

}

object ellipseCalculationMethod extends Enumeration {
  type ellipseCalculationMethod = Value
  val NUMERICAL, INTEGRAL, ANONYMOUS, HUDSON, RAMANUJAN1, RAMANUJAN2, HOLDERHIGH, HOLDERLOW, CANTRELL, UNKNOWN = Value

  def withNameWithDefault(s: String): Value =
    ellipseCalculationMethod.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
}
