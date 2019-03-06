package validation

import scalismo.geometry._3D
import scalismo.mesh._

object Hausdorff {

  def modifiedHausdorffDistance(m1: TriangleMesh[_3D], m2: TriangleMesh[_3D]): Double = {

    def allDistsBetweenMeshes(mm1: TriangleMesh[_3D], mm2: TriangleMesh[_3D]): Iterator[Double] = {

      for (ptM1 <- mm1.pointSet.points) yield {

        val cpM2 = mm2.pointSet.findClosestPoint(ptM1).point
        (ptM1 - cpM2).norm

      }

    }

    val d1 = allDistsBetweenMeshes(m1, m2)
    val d2 = allDistsBetweenMeshes(m2, m1)

    Math.max(d1.sum / m1.pointSet.numberOfPoints, d2.sum / m2.pointSet.numberOfPoints)
  }
}