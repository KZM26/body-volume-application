package measurement

import java.io.File
import java.nio.file.{Files, Paths}

import scalismo.geometry._3D
import scalismo.io.{LandmarkIO, MeshIO}
import scalismo.utils.Random

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

    // First load files
    val files = new File("data/inkreate/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}

    println("Data loaded")

    val meshNumber = 0
    val mesh = dataset(0)

    // TODO: Read Inkreate reference csv

    // Use landmarks or iterate through all or assume floor and ceiling and look for closest points to a point way higher/lower than them

    // WC. Keep z-axis same.
    // Move left from landmark and find closes point to surface
    // If same then move forward else continue
    // If moving forward, same until snap back to same point
    // Start moving right until point 90° to 1st (landmark) and 2nd points
    // Must log distances covered throughout till end
    // Multiply by 2 to get the WC.


  }

}