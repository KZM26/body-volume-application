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
    val measurementConfig = List("Experiments (e)", "Help (h)", "Return (r)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(measurementConfig.mkString("\n")).toLowerCase()
      input match {

        case "e" => // Start experiments
          pathFindingTest()
          heightTest()
          volumeTest()

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "r" => // Return
          return

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
    val writer = new PrintWriter(new File("data/measurement-test-result/heightTestDifferences.txt"))
    writer.write(md.toString + "\n")
    writer.write(mad.toString)
    writer.close()

    val csvFields = ListBuffer("Ground Truth", "Point Height")

    // Extract all data
    var allData = new ListBuffer[Seq[Any]]
    allData += csvHeight.toList
    allData += pointHeight.map{h => BigDecimal(h).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble}.toList
    val directory = "data/measurement-test-result/heightTest.csv"
    Utils.csvWrite(csvFields, allData.toList, directory)

    println("Results written to file")

  }

  private def pathFindingTest(): Unit = {

    // Square Distance
    // First load files
    // Extract meshs (.stl) and landmarks (.json)
    val sqFiles = new File("data/distance-test/").listFiles.filter(f => f.getName.contains("sq")).sortBy(_.getName)
    val trueDistance = sqFiles.filter(f => f.getName.contains(".stl")).map{f => f.getName.substring(0, f.getName.indexOf(".") - 2).toInt}
    val sqDataset = sqFiles.filter(f => f.getName.contains(".stl")).map{f => MeshIO.readMesh(f).get}
    val sqLandmarks = sqFiles.filter(f => f.getName.contains(".json")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get}
    var distance = new ListBuffer[Double]

    sqDataset.indices.map{i =>
      val mesh = sqDataset(i)
      val start = sqLandmarks(i).toIndexedSeq(0).point
      val end = sqLandmarks(i).toIndexedSeq(1).point
      distance += AStar.calculateDistance(mesh, start, end)
    }

    val sqFields = ListBuffer("Ground Truth", "True Distance")
    val sqDirectory = "data/measurement-test-result/squareDistanceTest.csv"
    var sqData = new ListBuffer[Seq[Any]]
    sqData += trueDistance.toList
    sqData += distance.toList
    Utils.csvWrite(sqFields , sqData.toList, sqDirectory)

    // Waist Circumference Test
    // The reference landmarks are based on shape 0 in the training set
    val inkreateRefFiles = new File("data/inkreate-ref/").listFiles.filter(f => f.getName.contains("json")).sortBy(_.getName)
    val inkreateRefLandmarks = inkreateRefFiles.map{f => LandmarkIO.readLandmarksJson[_3D](f).get}

    // First load files
    val bodyFiles = new File("data/inkreate/").listFiles.sortBy(_.getName)
    val bodyDataset = inkreateRefLandmarks.indices.map{f => MeshIO.readMesh(bodyFiles(f)).get}

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
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.ant").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.post").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)

      val girth: Double = AStar.calculateDistance(mesh, waistAnt, waistPost)
      girth * 2
    }

    val aStarXZGirth: IndexedSeq[Double] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.ant").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.post").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)

      val girth: Double = AStar.calculateXZPlaneDistance(mesh, waistAnt, waistPost)
      girth * 2
    }

    val aStarPlaneGirth: IndexedSeq[Double] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.ant").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.post").head.point).id
      val waistRtID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.rt").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      val waistRt = mesh.pointSet.point(waistRtID)

      val girth: Double = AStar.calculatePlaneDistance(mesh, waistAnt, waistPost, waistRt)
      girth * 2
    }

    // Get and IndexedSeq of IndexedSeq[Double]. Produces girth calculations for all
    val methods = List("Anonymous", "Cantrell", "HolderHigh", "HolderLow", "Hudson", "Integral", "Numerical", "Ramanujan1", "Ramanujan2")
    //val methods = List("Ramanujan2")
    val  ellipseGirth: IndexedSeq[IndexedSeq[Double]] = bodyDataset.indices.map{i =>
      // Get the points from the point ID
      val mesh = bodyDataset(i)
      val waistAntID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.ant").head.point).id
      val waistPostID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.post").head.point).id
      val waistRtID = mesh.pointSet.findClosestPoint(inkreateRefLandmarks(i).filter(p => p.id == "waist.rt").head.point).id

      val waistAnt = mesh.pointSet.point(waistAntID)
      val waistPost = mesh.pointSet.point(waistPostID)
      val waistRt = mesh.pointSet.point(waistRtID)

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
    val directory = "data/measurement-test-result/waistCircumferenceTest.csv"
    Utils.csvWrite(csvFields, allData.toList, directory)

    println("Results written to file")

  }

  private def volumeTest(): Unit = {
    val csvFields = ListBuffer("Ground Truth", "Measured Volume")

    val files = new File("data/distance-test/").listFiles
    val sqFiles = files.filter(f => f.getName.contains("sq")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val sqTrueVolume = sqFiles.map{f => math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val sqDataset = sqFiles.map{f => MeshIO.readMesh(f).get}
    val sqResult = sqDataset.map{mesh => getMeshVolume(mesh)}
    var sqData = new ListBuffer[Seq[Any]]
    sqData += sqTrueVolume.toList
    sqData += sqResult.toList
    val sqDirectory = "data/measurement-test-result/squareVolume.csv"
    Utils.csvWrite(csvFields, sqData.toList, sqDirectory)

    val spFiles = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("sp")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val spTrueVolume =  spFiles.map{f => (4.0/3.0) * math.Pi * math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val spDataset = spFiles.filter(f => f.getName.contains(".stl")).map{f => MeshIO.readMesh(f).get}
    val spResult = spDataset.map{mesh => getMeshVolume(mesh)}
    var spData = new ListBuffer[Seq[Any]]
    spData += spTrueVolume.toList
    spData += spResult.toList
    val spDirectory = "data/measurement-test-result/sphereVolume.csv"
    Utils.csvWrite(csvFields, spData.toList, spDirectory)

    val arFiles = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("shape")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val volFile = Source.fromFile("data/volume-test/volume.txt")
    // Read line by line using iterator. Drop first two lines
    val iter = volFile.getLines().toIndexedSeq
    var arTrue = new ListBuffer[Double]
    val arResult = iter.map{ info =>
      val shapeName = info.substring(0, info.indexOf("-"))
      arTrue += info.substring(info.indexOf("-") + 1, info.length).toDouble
      val mesh = MeshIO.readMesh(arFiles.filter(p => p.getName.containsSlice(shapeName)).head).get
      getMeshVolume(mesh)
    }
    var arData = new ListBuffer[Seq[Any]]
    arData += arTrue.toList
    arData += arResult.toList
    val arDirectory = "data/measurement-test-result/arbitraryVolume.csv"
    Utils.csvWrite(csvFields, arData.toList, arDirectory)

    val bodyFiles = new File("data/inkreate/").listFiles.sortBy(f => f.getName)
    val bodyDataset = bodyFiles.map{f => MeshIO.readMesh(f).get}
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val lines = csvSRC.getLines().drop(2).map(_.split(",")).toIndexedSeq
    val csvVolume = lines.map{row =>
      // Extract each volume (in column 111 = CY)
      // Multiple to convert to mm3 to match getMeshVolume output unit
      row(102).toDouble * 1e+6
    }
    val bodyResult = bodyDataset.map{mesh => getMeshVolume(mesh)}
    var bodyData = new ListBuffer[Seq[Any]]
    bodyData += csvVolume.toList
    bodyData += bodyResult.toList
    val bodyDirectory = "data/measurement-test-result/bodyVolume.csv"
    Utils.csvWrite(csvFields, bodyData.toList, bodyDirectory)

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