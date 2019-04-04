package measurement

import java.io.File
import java.nio.file.{Files, Paths}

import scalismo.common.PointId
import scalismo.geometry.{Landmark, _3D}
import scalismo.io.{LandmarkIO, MeshIO}
import scalismo.mesh.TriangleMesh
import scalismo.utils.Random
import tools.{AStar, Utils, EllipseMaster, ellipseCalculationMethod}
import tools.AStar.planeDistance

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Measurement {

  def start(): Unit = {
    val measurementConfig = List("Start measurements (s)", "Experiments (e)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(measurementConfig.mkString("\n")).toLowerCase()
      input match {

        case "s" => // Start measurements
          measure()

        case "e" => // Start experiments
          aStarTest()
          //volumeTest()

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

  private def measure() : Unit ={

    // Measure height
    // Height = ABS Diff between max and min vertex

    // Measure WC
    // Surface distance from waist preferred posterior. Maintain z coordinate

  }

  private def heightExperiment() : Unit ={

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
    if (!Files.exists(Paths.get("data/inkreate-ref/inkreateRefLandmarks.json"))) {
      println("Landmark file - inkreateRefLandmarks.json not found. Returning to previous menu")
      return
    }

    // The reference landmarks are based on shape 0 in the training set
    val inkreateRefLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/inkreate-ref/inkreateRefLandmarks.json")).get

    // First load files
    val files = new File("data/inkreate/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}
    val reference = dataset.head

    // Read file
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val iter = csvSRC.getLines().drop(2).map(_.split(","))
    // Collection for heights
    var csvHeight = new ListBuffer[Int]()
    // Extract each height (in column 2)
    while (iter.hasNext){
      csvHeight += iter.next()(2).toInt
    }

    println("Data loaded")

    // Extract the point IDs from the reference shape using the landmark coordinates
    // Reference landmark iterator
    // List buffer for points
    val it = inkreateRefLandmarks.seq.iterator
    var pointIDs = new ListBuffer[Int]()

    // Iterate, get point, extract pid from the reference shape
    while (it.hasNext){
      val pt = reference.pointSet.findClosestPoint(it.next().point).point
      val pid = reference.pointSet.pointId(pt)
      pointIDs += pid.get.id
    }

    val pointHeight : IndexedSeq[Int] = dataset.map{mesh =>

      val it2 = mesh.pointSet.points
      var pt = it2.next()
      var maxY = pt.y
      var minY = pt.y

      while (it2.hasNext){
        pt = it2.next()
        if (maxY < pt.y) maxY = pt.y
        if (minY > pt.y) minY = pt.y
      }

      val pointHeight : Int = Math.abs(maxY - minY).toInt
      pointHeight
    }.toIndexedSeq

    println("Done")
/*
    val meshNumber = 12
    val mesh = dataset(meshNumber)
    val landmarks = pointIDs.map{id => Landmark[_3D]("L_"+id, mesh.pointSet.point(PointId(id)))}

    // Use landmarks or iterate through all or assume floor and ceiling and look for closest points to a point way higher/lower than them
    val ptCrown = landmarks.toList.head.point
    val ptLtCalcaneousPost = landmarks.toList(1).point
    val landmarkHeightZ = Math.abs(ptCrown.z - ptLtCalcaneousPost.z)
    val landmarkHeightX = Math.abs(ptCrown.x - ptLtCalcaneousPost.x)
    val landmarkHeightY = Math.abs(ptCrown.y - ptLtCalcaneousPost.y)

    val it2 = mesh.pointSet.points
    var pt = it2.next()
    var maxY = pt.y
    var minY = pt.y

    while (it2.hasNext){

      pt = it2.next()
      if (maxY < pt.y) maxY = pt.y
      if (minY > pt.y) minY = pt.y

    }

    val pointHeight = Math.abs(maxY - minY)

    println("Landmark X Dist: " + landmarkHeightX.toString + " mm")
    println("Landmark Z Dist: " + landmarkHeightZ.toString + " mm")
    println("Landmark Y Height: " + landmarkHeightY.toString + " mm")
    println("Point Y Height: " + pointHeight.toString + " mm")
*/
    // WC. A* algorithm

  }

  private def aStarTest(): Unit = {

    scalismo.initialize()

    println("Scalismo initialised")

    // First load files
    // Extract meshs (.stl) and landmarks (.json)
    val sqFiles = new File("data/distance-test/").listFiles.filter(f => f.getName.contains("sq")).sortBy(_.getName)
    val trueDistance = sqFiles.filter(f => f.getName.contains(".stl")).map{f => f.getName.substring(0, f.getName.indexOf(".") - 2).toInt}
    val sqDataset = sqFiles.filter(f => f.getName.contains(".stl")).map{f => MeshIO.readMesh(f).get}
    val sqLandmarks = sqFiles.filter(f => f.getName.contains(".json")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get}
    var distance = new ListBuffer[Double]
    var sqResult = new ListBuffer[Boolean]

    for (i <- sqDataset.indices){
      val mesh = sqDataset(i)
      val start = sqLandmarks(i).toIndexedSeq(0).point
      val end = sqLandmarks(i).toIndexedSeq(1).point
      distance += AStar.calculateDistance(mesh, start, end)
      sqResult += distance(i) == trueDistance(i)
    }

    println("Square length test")
    println(sqResult.mkString("\n"))

    // Check if refLandmarks.json exists - Landmark file
    if (!Files.exists(Paths.get("data/inkreate-ref/inkreateRefLandmarks.json"))) {
      println("Landmark file - inkreateRefLandmarks.json not found. Returning to previous menu")
      return
    }

    // The reference landmarks are based on shape 0 in the training set
    val inkreateRefLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/inkreate-ref/inkreateRefLandmarks.json")).get

    // First load files
    val bodyFiles = new File("data/inkreate/").listFiles.sortBy(f => f.getName)
    val bodyDataset = bodyFiles.map{f => MeshIO.readMesh(f).get}
    val reference = bodyDataset.head

    // Read file
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val iter = csvSRC.getLines().drop(2).map(_.split(","))
    // Collection for heights
    var csvWaistGirth = new ListBuffer[Double]()
    // Extract each height (in column 51 = AZ)
    while (iter.hasNext){
      csvWaistGirth += iter.next()(51).toDouble
    }

    println("Data loaded")

    // Get point ID of the relevant landmarks using the reference mesh
    // Assume filter has one result. Get it
    val waistAntID = reference.pointSet.findClosestPoint(inkreateRefLandmarks.filter(p => p.id == "WaistAnt").head.point).id
    val waistPostID = reference.pointSet.findClosestPoint(inkreateRefLandmarks.filter(p => p.id == "WaistPost").head.point).id
    val waistRtID = reference.pointSet.findClosestPoint(inkreateRefLandmarks.filter(p => p.id == "WaistRt").head.point).id

    val aStarGirth: IndexedSeq[Double] = bodyDataset.map{ mesh =>
      // Get the points from the point ID
      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      val waistRt = mesh.pointSet.point(waistRtID)

      val girth: Double = AStar.calculateDistance(mesh, waistAnt, waistPost)
      girth * 2
    }

    // Get and IndexedSeq of IndexedSeq[Double]. Produces girth calculations for all
    val methods = List("Anonymous", "Cantrell", "HolderHigh", "HolderLow", "Hudson", "Integral", "Numerical", "Ramanujan1", "Ramanujan2")
    val  ellipseGirth: IndexedSeq[IndexedSeq[Double]] = bodyDataset.map{ mesh: TriangleMesh[_3D] =>
      // Get the points from the point ID
      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      val waistRt = mesh.pointSet.point(waistRtID)

      // List ellipse calculation methods
      // For each method calculate circumference
      val holder: IndexedSeq[Double] = methods.map{method: String =>
        EllipseMaster.calculateCircumference(mesh, waistAnt, waistPost, waistRt, ellipseCalculationMethod.withNameWithDefault(method))
      }.toIndexedSeq
      holder
    }

    val ellipseGirthTransposed = ellipseGirth.transpose.map{ f => f.toList}

    var csvFields = ListBuffer("Ground Truth", "AStar Basic")
    csvFields ++= methods

    // Extract all data
    var allData = new ListBuffer[Seq[Any]]
    allData += csvWaistGirth.toList
    allData += aStarGirth.toList
    allData ++= ellipseGirthTransposed
    val directory = "data/waistCircumferenceTest.csv"
    Utils.csvWrite(csvFields, allData.toList, directory)

    println("Results written to file")

  }

  private def volumeTest(): Unit = {

    scalismo.initialize()

    val files = new File("data/distance-test/").listFiles
    val sqFiles = files.filter(f => f.getName.contains("sq")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val sqTrueVolume = sqFiles.map{f => math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val sqDataset = sqFiles.map{f => MeshIO.readMesh(f).get}
    var  sqVol = new ListBuffer[Double]

    for (i <- sqDataset.indices){
      val mesh = sqDataset(i)
      sqVol += getMeshVolume(mesh)
    }

    val spFiles = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("sp")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val spTrueVolume =  spFiles.filter(f => f.getName.contains(".stl")).map{f => (4.0/3.0) * math.Pi * math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val spDataset = spFiles.filter(f => f.getName.contains(".stl")).map{f => MeshIO.readMesh(f).get}
    var spVol = new ListBuffer[Double]

    for (i <- spDataset.indices){
      val mesh = spDataset(i)
      spVol += getMeshVolume(mesh)
    }

    val trueShape1Vol = 96540.29
    val shape1 = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("Shape1")).filter(f => f.getName.contains(".stl")).sortBy(_.getName).map{f => MeshIO.readMesh(f).get}.head
    val shape1Vol = getMeshVolume(shape1)

    println("Done")

  }

  def getMeshVolume(mesh: TriangleMesh[_3D]): Double = {
    // Convert TriangleMesh[_3D] to vtkPolyData
    val vtkMesh = scalismo.utils.MeshConversion.meshToVtkPolyData(mesh)
    // Create new vtk mass properties object
    val mass = new vtk.vtkMassProperties()
    // Add mesh to the properties object
    mass.AddInputData(vtkMesh)
    // Return volume
    mass.GetVolume()
  }


}