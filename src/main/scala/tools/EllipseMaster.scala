package tools

import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh

import scala.collection.mutable
import scala.math.Ordering
import scala.collection.mutable.ListBuffer

object  EllipseMaster {

  def calculateCircumference(mesh: TriangleMesh[_3D], ellipsePost: Point[_3D], ellipseAnt: Point[_3D], ellipseSide: Point[_3D]): IndexedSeq[Double] = {

    // Find true points on mesh
    val truePost = mesh.pointSet.findClosestPoint(ellipsePost).point
    val trueAnt = mesh.pointSet.findClosestPoint(ellipseAnt).point
    val trueSide = mesh.pointSet.findClosestPoint(ellipseSide).point

    // Calculate a and b for ellipse
    // Assume right ellipse (a > b)
    // For a find distance between
    val a = distance(trueSide, (truePost - trueAnt).toPoint)
    val b = distance(truePost, trueAnt)/2
    val n = 100

    var circumference = new ListBuffer[Double]
    circumference += numerical(a, b, n)
    circumference += integral(a, b, n)

    circumference.toIndexedSeq
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
  // TODO: Fix this vs ground truth
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
    val e = math.sqrt(1 - minor*minor/major*major)
    val e2 = e * e

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
  // TODO: Fix this vs ground truth
  def cantrell (a: Double, b: Double): Double = {
    val e = math.sqrt(1 - b*b/a*a)
    var s = 0.0

    if (e < 0.99){
      s = (3 * math.Pi - 8)/(8 - 2* math.Pi)
    }
    else{
      s = math.log(2.0)/math.log(2.0/(4 -  math.Pi))
    }

    4 * (a + b) - (2 * (4 - math.Pi) * a * b)/math.pow(math.pow(a, s) + math.pow(b, s), 1/s)
  }

  // Exact approximation
  // TODO: Fix this vs ground truth
  def exact (a: Double, b: Double, n: Int): Double = {

    // Check what is major and what is minor for axis
    var major = 0.0
    var minor = 0.0
    if (a >= b){
      major = a
      minor = b
    }
    else{
      major = b
      minor = a
    }

    val e = math.sqrt(1 - minor*minor/major*major)

    var len = 0.0
    for (i <- 1 until n){
      val num = -math.pow(e, 2 * i) * math.pow(factorial(2 * i).toInt/math.pow(math.pow(2, i) * factorial(i).toInt, 2), 2)
      len += num/(2 * i - 1)
    }
    len * 2 * math.Pi * major
  }

  // Bessel approximation
  // TODO: Check if it's worth it. Takes way too long
  def bessel (a: Double, b: Double, n: Int): Double = {
    var len = 0.0

    // Check what is major and what is minor for axis
    var major = 0.0
    var minor = 0.0
    if (a >= b){
      major = a
      minor = b
    }
    else{
      major = b
      minor = a
    }

    val h = math.pow(major - minor, 2)/math.pow(major + minor, 2)
    len += 1 + h/4

    for (i <- 2 until n){
      len += math.pow(factorial(factorial(2 * i - 3).toInt).toInt/(math.pow(2, i) * factorial(i).toInt), 2) * math.pow(h, i)
    }
    len * math.Pi * (major + minor)
  }

  // Recursive factorial implementation
  // Code from: https://medium.com/@mattmichihara/scala-tail-call-optimization-f853b8f295dc
  def factorial(n: Int, accum: BigInt = 1): BigInt = {
    if (n == 0) {
      accum
    } else {
      factorial(n - 1, n * accum)
    }
  }

  private def distance(a: Point[_3D], b: Point[_3D]): Double = {
    math.sqrt(math.pow(a.x - b.x,2) + math.pow(a.y - b.y,2) + math.pow(a.z - b.z,2))
  }

}
