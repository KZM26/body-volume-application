package preprocessor

import java.io.File
import java.nio.file.{Files, Paths}

import scalismo.common.PointId
import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI
import scalismo.io._
import scalismo.geometry._
import scalismo.mesh.TriangleMesh
import scalismo.registration._

import scala.collection.mutable.ListBuffer

object Preprocessor {

  private val refLandmarkPath: String =  "data/ref-landmarks/refLandmarks.json"

  def start(): Unit = {

    val preprocessorConfig = List("Landmark reference shape (l)", "Align to Ref (a)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n")).toLowerCase()
      input match {

        case "l" => // Start landmarking
          landmarkRef()

        case "a" => // Start Alignment
          align()

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
    if (!Files.exists(Paths.get(refLandmarkPath))) {
      println("Landmark file - refLandmarks.json not found. Returning to previous menu")
      return
    }

    // If exists, start alignment

    // First load files
    val files = new File("data/training/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}

    println("Data loaded")

    // The reference landmarks are based on shape 0 in the training set
    val refLandmarks = LandmarkIO.readLandmarksJson[_3D](new File(refLandmarkPath)).get
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

    // Get the shapes to align to the reference

    println("Reference landmarks extracted")

    println("Starting alignment")

    val centreOfMass = computeCentreOfMass(reference)

    val refLandmarksProper = pointIDs.map{id => Landmark("L_"+id, reference.pointSet.point(PointId(id))) }
    // Find a rigid transform for each in the to align set and apply it
    val alignedSet = dataset.map{ mesh =>
      val landmarks = pointIDs.map{id => Landmark[_3D]("L_"+id, mesh.pointSet.point(PointId(id)))}
      val rigidTrans = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, refLandmarksProper, centreOfMass)
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

  def computeCentreOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
    val normFactor = 1.0 / mesh.pointSet.numberOfPoints
    mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
  }
}
