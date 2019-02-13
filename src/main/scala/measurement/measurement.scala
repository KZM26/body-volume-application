package measurement

import java.io.File
import java.nio.file.{Files, Paths}

import scalismo.common.PointId
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, MeshIO}
import scalismo.utils.Random

import scala.collection.mutable.ListBuffer

class measurement {

  def start(): Unit = {
    val measurementConfig = List("Start measurements (s)", "Experiments (e)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(measurementConfig.mkString("\n")).toLowerCase()
      input match {

        case "s" => // Start measurements
          this.measure()

        case "e" => // Start experiments
          this.experiment()

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "q" => // Quit
          input = scala.io.StdIn.readLine("Are you sure you want to quit (y/n)?\n").toLowerCase()
          input match {

            case "y" => // Yes
              sys.exit(0)

            case _ => // Any
              println("That ain't it chief\n")
          }

        case _ => // Any
          println("That ain't it chief\n")
      }
    }
  }

  def measure() : Unit ={

    // Measure height
    // Height = ABS Diff between max and min vertex

    // Measure WC
    // Surface distance from waist preferred posterior. Maintain z coordinate

  }

  def experiment() : Unit ={

    // Measure height of all and WC of all

    /*
    // Check if refLandmarks.json exists - Landmark file
    if (!Files.exists(Paths.get("data/ref_landmarks/inkreateRefLandmarks.json"))) {
      println("Landmark file - inkreateRefLandmarks.json not found. Returning to previous menu")
      return
    }

    // The reference landmarks are based on shape 0 in the training set
    val inkreateRefLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/ref_landmarks/inkreateRefLandmarks.json")).get
    */

    // If exists, start alignment
    // Initialise Scalismo
    scalismo.initialize()

    println("Scalismo initialised")

    // Check if refLandmarks.json exists - Landmark file
    if (!Files.exists(Paths.get("data/inkreate_ref/inkreateRefLandmarks.json"))) {
      println("Landmark file - inkreateRefLandmarks.json not found. Returning to previous menu")
      return
    }

    // The reference landmarks are based on shape 0 in the training set
    val inkreateRefLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/inkreate_ref/inkreateRefLandmarks.json")).get

    // First load files
    val files = new File("data/inkreate/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}
    val reference = dataset.head

    println("Data loaded")

    // Extract the point IDs from the reference shape using the landmark coordinates
    // Reference landmark iterator
    // List buffer for points
    var it = inkreateRefLandmarks.seq.iterator
    var pointIDs = new ListBuffer[Int]()

    // Iterate, get point, extract pid from the reference shape
    while (it.hasNext){
      val pt = reference.pointSet.findClosestPoint(it.next().point).point
      val pid = reference.pointSet.pointId(pt)
      pointIDs += pid.get.id
    }

    val meshNumber = 0
    val mesh = dataset(meshNumber)

    val landmarks = pointIDs.map{id => Landmark[_3D]("L_"+id, mesh.pointSet.point(PointId(id)))}

    // TODO: Read Inkreate reference csv

    // Use landmarks or iterate through all or assume floor and ceiling and look for closest points to a point way higher/lower than them
    val ptCrown = landmarks.toList.head.point
    val ptLtCalcaneousPost = landmarks.toList(1).point
    val landmarkHeightZ = Math.abs(ptCrown.z - ptLtCalcaneousPost.z)
    val landmarkHeightX = Math.abs(ptCrown.x - ptLtCalcaneousPost.x)
    val landmarkHeightY = Math.abs(ptCrown.y - ptLtCalcaneousPost.y)

    println("Landmark X Dist: " + landmarkHeightX.toString + " metre")
    println("Landmark Y Height: " + landmarkHeightY.toString + " metre")
    println("Landmark Z Dist: " + landmarkHeightZ.toString + " metre")

    // WC. Keep z-axis same.
    // Move left from landmark and find closes point to surface
    // If same then move forward else continue
    // If moving forward, same until snap back to same point
    // Start moving right until point 90° to 1st (landmark) and 2nd points
    // Must log distances covered throughout till end
    // Multiply by 2 to get the WC.

  }

}