package measurement

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import scalismo.geometry.{Landmark, Point, _3D}
import scalismo.io.{LandmarkIO, MeshIO}
import scalismo.mesh.TriangleMesh
import tools.{AStar, EllipseMaster, Utils, ellipseCalculationMethod}
import measurement.Measurement.sexEnum.sexEnum

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
          //TODO

        case "e" => // Start experiments
          pathFindingTest()
          heightTest()

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

  def measurePointHeight(mesh: TriangleMesh[_3D]): Double ={

    // Measure height
    // Height = ABS Diff between max and min vertex
    val meshSeq = mesh.pointSet.points.toIndexedSeq

    val maxY = meshSeq.maxBy(f => f.y).y
    val minY = meshSeq.minBy(f => f.y).y

    maxY - minY
  }

  private def heightTest() : Unit ={

    // First load files
    val bodyFiles = new File("data/inkreate/").listFiles
    val dataset = bodyFiles.map{f => MeshIO.readMesh(f).get}

    // Read file
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val iter = csvSRC.getLines().drop(2).map(_.split(","))

    // Collection for heights
    val csvHeight = iter.toIndexedSeq.map{l =>
      l(2).toInt
    }

    val pointHeight : IndexedSeq[Double] = dataset.map{mesh =>
      measurePointHeight(mesh)
    }.toIndexedSeq

    println("Done")

    val (md, mad) = Utils.getDifferences(csvHeight.map{f => f.toDouble}, pointHeight)
    val writer = new PrintWriter(new File("data/heightTestDifferences.txt"))
    writer.write(md.toString + "\n")
    writer.write(mad.toString)
    writer.close()

    val csvFields = ListBuffer("Ground Truth", "Point Height")

    // Extract all data
    var allData = new ListBuffer[Seq[Any]]
    allData += csvHeight.toList
    allData += pointHeight.map{h => BigDecimal(h).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble}.toList
    val directory = "data/heightTest.csv"
    Utils.csvWrite(csvFields, allData.toList, directory)

    println("Results written to file")

  }

  private def pathFindingTest(): Unit = {

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
    val inkreateRefFiles = new File("data/inkreate-ref/").listFiles.filter(f => f.getName.contains("json")).sortBy(_.getName)
    val inkreateRefLandmarks = inkreateRefFiles.map{f => LandmarkIO.readLandmarksJson[_3D](f).get}

    // First load files
    val bodyFiles = new File("data/inkreate/").listFiles.sortBy(f => f.getName)
    val bodyDataset = inkreateRefLandmarks.indices.map{i => MeshIO.readMesh(bodyFiles(i)).get}

    // Read out the same number of values from csv as there are body files
    // Read file
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val iter = csvSRC.getLines().drop(2).map(_.split(","))
    // Collection for heights
    var csvWaistGirth = new ListBuffer[Double]()
    // Extract each height (in column 51 = AZ)
    for (i <- bodyDataset.indices){
      csvWaistGirth += iter.next()(51).toDouble
    }

    println("Data loaded")

    // Get point ID of the relevant landmarks using the reference mesh
    // Assume filter has one result. Get it


    val aStarGirth: IndexedSeq[Double] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistAnt").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistPost").head.point).id
      val waistRtID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistRt").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      //val waistRt = mesh.pointSet.point(waistRtID)

      val girth: Double = AStar.calculateDistance(mesh, waistAnt, waistPost)
      girth * 2
    }

    val aStarXZGirth: IndexedSeq[Double] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistAnt").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistPost").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)

      val girth: Double = AStar.calculateXZPlaneDistance(mesh, waistAnt, waistPost)
      girth * 2
    }

    val aStarPlaneGirth: IndexedSeq[Double] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistAnt").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistPost").head.point).id
      val waistRtID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistRt").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      val waistRt = mesh.pointSet.point(waistRtID)

      val girth: Double = AStar.calculatePlaneDistance(mesh, waistAnt, waistPost, waistRt)
      girth * 2
    }

    // Get and IndexedSeq of IndexedSeq[Double]. Produces girth calculations for all
    //val methods = List("Anonymous", "Cantrell", "HolderHigh", "HolderLow", "Hudson", "Integral", "Numerical", "Ramanujan1", "Ramanujan2")
    val methods = List("Ramanujan2")
    val  ellipseGirth: IndexedSeq[IndexedSeq[Double]] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistAnt").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistPost").head.point).id
      val waistRtID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "WaistRt").head.point).id

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

    var csvFields = ListBuffer("Ground Truth", "AStar Basic", "AStar Projection Plane", "AStar XZ Plane")
    csvFields ++= methods

    // Extract all data
    var allData = new ListBuffer[Seq[Any]]
    allData += csvWaistGirth.toList
    allData += aStarGirth.toList
    allData += aStarPlaneGirth.toList
    allData += aStarXZGirth.toList
    allData ++= ellipseGirthTransposed
    val directory = "data/waistCircumferenceTest.csv"
    Utils.csvWrite(csvFields, allData.toList, directory)

    println("Results written to file")

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

  def getBodyFatPercentage(mesh: TriangleMesh[_3D], height: Double, mass: Double, age: Int, sex: sexEnum): Double = {
    // Get volume in litres. Scale up from mm3 to l
    val bodyVolumeRaw = getMeshVolume(mesh) * 1e+3
    var frc = 0.0
    var vt = 0.0

    // Set functional residual capacity (FRC) based on height, age, and sex. Set tidal volume (VT) based on sex
    if (sex == sexEnum.FEMALE){
      frc = 0.0360 * height + 0.0031 * age - 3.182
      vt = 0.7
    }
    else {
      frc = 0.0472 * height + 0.0090 * age - 5.290
      vt = 1.2
    }

    // Determine thoracic gas volume (TGV)
    val tgv = frc + 0.5 * vt
/*
    val meshArea = mesh.area * 1e+4
    val k = -4.67 * 10e-5
    val SAA = k * meshArea
*/
    val bodyVolumeCorrected = bodyVolumeRaw + tgv
    val bodyDensity = mass/bodyVolumeCorrected

    // Determine body fat percentage using Siri or Brožek based on BMI
    // Assume height given in meters
    var bodyFat = 0.0
    val bmi = mass/(height * height)

    if (18.5 <= bmi && bmi <= 30){
      // Siri, 1956
      bodyFat = 100 * ((4.95/bodyDensity) - 4.50)
    }
    else {
      // Brožek 1963
      bodyFat = 100 * ((4.57/bodyDensity) - 4.142)
    }

    math.abs(bodyFat)
  }

  def getWaistCircumference(mesh: TriangleMesh[_3D], waistAnt: Point[_3D], waistPost: Point[_3D]): Double = {
    AStar.calculateXZPlaneDistance(mesh, waistAnt, waistPost) * 2
  }

  object sexEnum extends Enumeration {
    type sexEnum = Value
    val FEMALE, MALE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      sexEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
  }

}