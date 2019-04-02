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

  // Numerical approximation of circumference
  def numerical(a: Double, b: Double, n: Int): Double ={
    var trueLen = 0.0
    var xLast = 0.0
    var yLast = 0.0

    for (i <- 0 to n){
      val theta = 0.5 * math.Pi * i/n.toDouble
      val x = a * math.cos(theta)
      val y = b * math.sin(theta)

      if (i > 0){
        val xDiff = x - xLast
        val yDiff = y - yLast
        trueLen += math.sqrt(xDiff*xDiff + yDiff*yDiff)
      }

      xLast = x
      yLast = y

    }
    trueLen * 4
  }

  // integral approximation of circumference
  def integral(a: Double, b: Double, n: Int): Double ={
    var trueLen = 0.0

    val dt = 0.5 * math.Pi/n.toDouble

    for (i <- 0 to n){
      val theta = 0.5 * math.Pi * (i + 0.5)/n.toDouble
      trueLen += math.sqrt(1 - math.E * math.E * math.sin(theta) * math.sin(theta)) * dt

    }
    trueLen * 4 * a
  }



  private def distance(a: Point[_3D], b: Point[_3D]): Double = {
    math.sqrt(math.pow(a.x - b.x,2) + math.pow(a.y - b.y,2) + math.pow(a.z - b.z,2))
  }

}
