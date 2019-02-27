package preprocessor

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import javafx.scene.shape.TriangleMesh
import scalismo.common.PointId
import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI
import scalismo.io._
import scalismo.geometry._
import scalismo.mesh.TriangleMesh3D
import scalismo.registration._
import scalismo.statisticalmodel.dataset.{DataCollection, DataItem}
import scala.collection.mutable.ListBuffer

class Preprocessor {

  def start(): Unit = {

    val preprocessorConfig = List("Landmark reference shape (l)", "Align to Ref (a)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n")).toLowerCase()
      input match {

        case "l" => // Start landmarking
          this.landmarkRef()

        case "a" => // Start Alignment
          this.align()

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

  private def landmarkRef(): Unit = {

    // Initialise Scalismo
    scalismo.initialize()

    println("Scalismo Viewer will now open\nSelect the landmarks and save them as refLandmarks.json in the data directory")

    // create a visualisation window
    val ui = ScalismoUI()

    // Load the reference shape
    val mesh = MeshIO.readMesh(new File("data/training/tr_gt_000.stl")).get

    // Display it
    val group = ui.createGroup("object-1")
    val meshView = ui.show(group, mesh, "body_0")

    // val files = new File("data/training/").listFiles
    // val dataset = files.map{f => MeshIO.readMesh(f).get}
    // (0 until dataset.size).foreach{i => show(dataset(i),"body_"+i)}

  }

  private def align(): Unit = {

    // Check if refLandmarks.json exists - Landmark file
    if (!Files.exists(Paths.get("data/ref_landmarks/refLandmarks.json"))) {
      println("Landmark file - refLandmarks.json not found. Returning to previous menu")
      return
    }

    // If exists, start alignment
    // Initialise Scalismo
    scalismo.initialize()

    println("Scalismo initialised")

    // First load files
    val files = new File("data/training/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}

    println("Data loaded")

    // The reference landmarks are based on shape 0 in the training set
    val refLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/ref_landmarks/refLandmarks.json")).get
    val reference = dataset.head

    // Extract the point IDs from the reference shape using the landmark coordinates
    // Reference landmark iterator
    // List buffer for points
    val it = refLandmarks.seq.iterator
    var pointIDs = new ListBuffer[Int]()

    // Iterate, get point, extract pid from the reference shape
    while (it.hasNext){
      val pt = it.next().point
      val pid = reference.pointSet.pointId(pt)
      pointIDs += pid.get.id
    }

    // Write to file
    /**
    val writer = new PrintWriter(new File("data/ref_landmarks/refLandmarksPoints.txt"))
    writer.write(pointIDs.mkString("\n"))
    writer.close()
**/
    // Get the shapes to align to the reference
    val toAlign = dataset.tail

    println("Reference landmarks extracted")

    println("Starting alignment")

    val refLandmarksProper = pointIDs.map{id => Landmark("L_"+id, reference.pointSet.point(PointId(id))) }
    // Find a rigid transform for each in the to align set and apply it
    val alignedSet = dataset.map{ mesh =>
      val landmarks = pointIDs.map{id => Landmark[_3D]("L_"+id, mesh.pointSet.point(PointId(id)))}
      // TODO: Figure out what the centre should actually be
      val rigidTrans = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, refLandmarksProper, Point3D(0, 0, -10))
      mesh.transform(rigidTrans)
    }

    println("Alignment complete. Writing to file (JK)")
    // Write the aligned data to file
    // Manual inspection
    val ui = ScalismoUI()
    alignedSet.indices.foreach{i => ui.show(alignedSet(i),"b_"+i)}
    // Array.tabulate(alignedSet.length){i => MeshIO.writeMesh(alignedSet(i), new File("data/training_aligned/tr_gt0" + i.toString + "0.stl"))}
    println("Writing complete")

  }
}
