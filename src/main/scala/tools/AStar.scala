package tools

import scalismo.geometry.{Point, _3D}
import scalismo.mesh.TriangleMesh

import scala.collection.mutable
import scala.math.Ordering
import scala.collection.mutable.ListBuffer

object AStar {

  def calculateDistance(mesh: TriangleMesh[_3D], start: Point[_3D], end: Point[_3D]): Double = {

    // Find true start and end as landmark point != point on mesh
    val trueStart = mesh.pointSet.findClosestPoint(start).point
    val trueEnd = mesh.pointSet.findClosestPoint(end).point

    val (cameFrom, cost)= aStarSearch(mesh, trueStart, trueEnd)
    val path = reconstructPath(cameFrom, trueStart, trueEnd)
    pathDistance(path)

  }

  // Define ordering case class and metric extraction
  private case class PointCost(point: Point[_3D], cost: Double)
  private def getPointCostCost(pc: PointCost) = -pc.cost

  // Define case class to couple origin and destination for came from
  private case class PointFrom(ori: Point[_3D], des: Point[_3D])
  private def getPointFromOrigin(pf: PointFrom): Point[_3D] = pf.ori

  /**
    * A* search function
    * Based on https://www.redblobgames.com/pathfinding/a-star/implementation.html
    * Adapted for Scala and Scalismo Meshes
    *
    * Parameters:
    *    -   `mesh` the mesh (graph)
    *    -   `start` the start point
    *    -   `end` the endpoint
    * Returns:
    *    -  Euclidean distance from `a` to `b`
    */
  private def aStarSearch(mesh: TriangleMesh[_3D], start: Point[_3D], end: Point[_3D]): Tuple2[ListBuffer[PointFrom], ListBuffer[PointCost]] = {

    val frontier: mutable.PriorityQueue[PointCost] = new mutable.PriorityQueue[PointCost]()(Ordering.by(getPointCostCost))
    frontier.enqueue(PointCost(start, 0))

    val cameFrom = new ListBuffer[PointFrom]()
    val costSoFar = new ListBuffer[PointCost]()

    cameFrom += PointFrom(start, start)
    costSoFar += PointCost(start, 0)

    while (frontier.nonEmpty) {
      val current = frontier.dequeue()

      // Check if current point matches destination. If so, break
      if (pointsEqual(current.point, end))
        return  (cameFrom, costSoFar)

      // Extract start pointID and find IndexedSeq of neighbours from the original mesh
      // Assumption is that start point is on the mesh.
      val currentPointID = mesh.pointSet.findClosestPoint(current.point).id
      val neighbours = mesh.triangulation.adjacentPointsForPoint(currentPointID)
      // Iterator of pointIDs
      val neighboursIterator = neighbours.iterator

      // Iterate through a point's neighbours to find costs
      while(neighboursIterator.hasNext) {
        val next = mesh.pointSet.point(neighboursIterator.next())

        // Search through cost so far for a PointCost containing the current point and get the cost of that PointCost
        var newCost: Double = 0.0
        val cSoFar = costSoFar.find((p: PointCost) => pointsEqual(current.point, p.point))
        if (cSoFar.isDefined){
          newCost += cSoFar.get.cost
        }
        // Add the distance to the first neighbour in the neighbour iterator. Found using the mesh and the pointID from the iterator
        newCost += heuristic(current.point, next)

        // If next it not in costSoFar (ie: an unseen point) OR newCost is less than costSoFar to next
        val cPoint = costSoFar.find((p: PointCost) => pointsEqual(next, p.point))
        if (cPoint.isEmpty || (newCost < cPoint.get.cost)) {
          // Add cost to next to costSoFar as CostPoint
          // Check if the current point is in costSoFar, if true replace
          if (cPoint.isDefined){
            costSoFar -= cPoint.get
          }

          costSoFar += PointCost(next, newCost)

          // Add to the frontier, the next point and priority = cost to get there + distance from end (heuristic)
          val priority = newCost + heuristic(end, next)
          frontier.enqueue(PointCost(next, priority))

          // Add a point from for current and next (ie: where I am and where I'm going next)
          // Sane as for costSoFar, check & update
          val cfPoint = cameFrom.find((p: PointFrom) => pointsEqual(next, p.ori))
          if (cfPoint.isDefined) {
            cameFrom -= cfPoint.get
          }
            cameFrom += PointFrom(current.point, next)
        }
      }
    }
    (cameFrom, costSoFar)
  }

  /**
    * Function to reconstruct path from A* algorithm
    *
    * Parameters:
    *    -   `pointFrom` list of PointFrom objects
    *    -   `start` the start point
    *    -   `end` the endpoint
    * Returns:
    *    -  List of points representing the path
    */
  private def reconstructPath(cameFrom: ListBuffer[PointFrom], start: Point[_3D], end: Point[_3D]): ListBuffer[Point[_3D]] = {
    var current = end
    var path = new ListBuffer[Point[_3D]]
    
    // Start at the end and work backwards till current is the start
    while (!pointsEqual(current, start)) {
      // Add current point to path
      path += current
      // Search cameFrom using destination that matches current. When found get the origin and make it current (move to that point)
      current = cameFrom.find((p: PointFrom) => pointsEqual(p.des, current)).get.ori
    }

    // Add start to path and return a reversed path
    path += start
    path.reverse
  }

  /**
    * Calculate distance from path
    *
    * Parameters:
    *    -   `path` list of points representing the path

    * Returns:
    *    -  Length of path
    */
  private def pathDistance(path: ListBuffer[Point[_3D]]): Double = {

    var length: Double = 0.0

    for (i <- 0 until (path.length - 1))
      length += this.heuristic(path(i), path(i + 1))

    length
  }

  /**
    * The function used in estimating the heuristic
    *
    * Parameters:
    *    -   `a` the start point
    *    -   `b` the end point
    * Returns:
    *    -  Euclidean distance from `a` to `b`
    */
  private def heuristic(a: Point[_3D], b: Point[_3D]): Double = {
    math.sqrt(math.pow(a.x - b.x,2) + math.pow(a.y - b.y,2) + math.pow(a.z - b.z,2))
  }

  /**
    * Checks if points are the same
    *
    * Parameters:
    *    -   `a` the first point
    *    -   `b` the second point
    * Returns:
    *    -  Boolean indicating if x,y,z of points are the same
    */
  private def pointsEqual(a: Point[_3D], b: Point[_3D]): Boolean = {
    if (a == null || b == null){
      return false
    }
    (a.x == b.x) && (a.y == b.y) && (a.z == b.z)
  }

}
