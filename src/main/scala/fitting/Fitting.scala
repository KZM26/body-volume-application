package fitting

import java.io.File

import javax.imageio.ImageIO

import scala.io.Source
import breeze.linalg.{DenseMatrix, DenseVector}
import measurement.Measurement
import fitting.Fitting.orientationEnum.orientationEnum
import scalismo.common._
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.geometry._
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO}
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.ui.api.{Group, ScalismoUI}
import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.faces.parameters.ViewParameter
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.kernels.{DiagonalKernel, GaussianKernel, MatrixValuedPDKernel, PDKernel}
import scalismo.numerics.RandomMeshSampler3D

import tools.Utils.sexEnum
import tools.Utils.sexEnum.sexEnum
import validation.Hausdorff

object Fitting {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def start(): Unit = {
    val fittingConfig = List("Start image fitting (f)", "Experiments (e)", "Help (h)", "Return (r)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(fittingConfig.mkString("\n")).toLowerCase()
      input match {

        case "s" => // Start fitting
          //TODO

        case "e" => // Start experiments
          val l = IndexedSeq(50.0)
          val s = IndexedSeq(200.0)
          for (i <- l) {
            for (j <- s) {
              fittingTestLandmark(i, j)
              println("L = " + i.toString + " Sig = " + j.toString)
            }
          }
          //fittingTest()

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "r" => // Return
          return

        case _ => // Any
          println("That ain't it chief\n")
      }
    }
  }



  def fittingTestLandmark(l: Double, s: Double): Unit = {

    val maleModel = StatismoIO.readStatismoMeshModel(new File("data/maleFBM.h5")).get
    val femaleModel = StatismoIO.readStatismoMeshModel(new File("data/femaleFBM.h5")).get

    val augMaleModel = augmentFittingModel(maleModel, l, s)
    val augFemaleModel = augmentFittingModel(femaleModel, l, s)

    val maleModelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/male.json")).get.sortBy{lm => lm.id}
    val femaleModelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/female.json")).get.sortBy{lm => lm.id}

    val maleModelContainer = modelContainer(augMaleModel, maleModelLm)
    val femaleModelContainer = modelContainer(augFemaleModel, femaleModelLm)

    val meshLm = LandmarkIO.readLandmarksJson[_3D](new File("data/mpi-training/mpi.json")).get.sortBy{lm => lm.id}
    val meshes = new File("data/mpi-training/").listFiles.filter{f => f.getName.contains("stl") && !f.getName.contains("arm")}.sortBy{f => f.getName}.take(2).map{f => MeshIO.readMesh(f).get}

    val meshReference = meshes(0)

    val meshWaistAntID = meshes(0).pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.anterior").head.point).id
    val meshWaistPostID = meshes(0).pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.posterior").head.point).id

    val realHeight: IndexedSeq[Double] = meshes.map{mesh => Measurement.measurePointHeight(mesh)}//Source.fromFile("data/image-landmarks/height.txt").getLines().toIndexedSeq.map{h => h.toDouble}
    val realWC: IndexedSeq[Double] = meshes.map{mesh => Measurement.getWaistCircumference(mesh, mesh.pointSet.point(meshWaistAntID), mesh.pointSet.point(meshWaistPostID)) * 1e3}
    val realVol: IndexedSeq[Double] = meshes.map{mesh => Measurement.getMeshVolume(mesh) * 1e3}

    val sex: IndexedSeq[String] = Source.fromFile("data/image-landmarks/sex.txt").getLines().toIndexedSeq
    val tlmsLandmarks = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("tlms")}.sortBy{f => f.getName}
    val imageFiles = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("png")}.sortBy{f => f.getName}

    val imgLmFront: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i) * 1e3
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      // Scale image. Scale to correct distance and coordinate system
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = Vector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val sideImages = imageFiles.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val imgLmSide: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val targetLmSide: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmSide.indices.map{i =>
      val lmHeightSide = imgLmSide(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmSide(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = sideImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightSide)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmSide(i).map{l =>
        val newPoint = Vector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    // Load data into container object
    val targets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, sexEnum.withNameWithDefault(sex(i)))
      var fittingModelContainer: fittingModelContainer = null
      if (fitData.sex == sexEnum.MALE){
        fittingModelContainer = new fittingModelContainer(fitData, maleModelContainer)
      }
      else {
        fittingModelContainer = new fittingModelContainer(fitData, femaleModelContainer)
      }

      fittingModelContainer
    }

    //println("Data loaded. Starting fitting")

    val results: IndexedSeq[(TriangleMesh[_3D], Double, Double, Double)] = targets.map{target =>
      val posterior = landmarkPosterior(target)
      val waistAntID = target.modelCon.model.referenceMesh.pointSet.findClosestPoint(target.modelCon.modelLM.filter(p => p.id == "waist.anterior").head.point).id
      val waistPostID = target.modelCon.model.referenceMesh.pointSet.findClosestPoint(target.modelCon.modelLM.filter(p => p.id == "waist.posterior").head.point).id
      val bestFit = posterior.mean
      val heightPosterior = Measurement.measurePointHeight(bestFit)
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val result: (TriangleMesh[_3D], Double, Double, Double) = (bestFit, heightPosterior, wcPosterior, volPosterior)
      result
    }
    val ui = ScalismoUI()
    val resultGroups = IndexedSeq[Group](ui.createGroup("Male"), ui.createGroup("Female"))

    results.indices.map{i =>
      val original = meshes(i)
      val result = results(i)._1

      val meshLms = meshLm.map{lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id))}
      val resultLm = maleModelLm.map{lm => new Landmark[_3D](lm.id, result.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id))}

      val cutOriginal = original//meshFingerClip(original, meshLms.toIndexedSeq)
      val cutResult = meshArmClip(result, resultLm.toIndexedSeq)

      val alignedCutOriginala = cutOriginal.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(result)))
      val ameshLms = meshLm.map{lm => new Landmark[_3D](lm.id, alignedCutOriginala.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id))}
      val alignedCutOriginal = meshFingerClip(alignedCutOriginala, ameshLms.toIndexedSeq)

      println(MeshMetrics.avgDistance(alignedCutOriginal, cutResult))
      println(Hausdorff.modifiedHausdorffDistance(alignedCutOriginal, cutResult))


      ui.show(resultGroups(i), cutResult, "Fitted: " + i.toString)
      ui.show(resultGroups(i), alignedCutOriginal, "Original: " + i.toString)
    }

    val bestFitHeightPosterior = results.map{result => result._2}
    val bestFitWCPosterior = results.map{result => result._3}
    val bodyVolPosterior = results.map{result => result._4}
    println("Height Posterior")
    println(bestFitHeightPosterior, realHeight)
    println("WC Posterior")
    println(bestFitWCPosterior, realWC)
    println("Vol Posterior")
    println(bodyVolPosterior, realVol)
  }

  def landmarkPosterior(fittingModel: fittingModelContainer): StatisticalMeshModel = {

    val model = fittingModel.modelCon.model
    val modelLm = fittingModel.modelCon.modelLM
    val targetLmFront = fittingModel.fitData.lmFront
    val targetLmSide = fittingModel.fitData.lmSide

    val targetLmFront3D = targetLmFront.map{l => new Landmark[_3D](l.id, Point3D(l.point.x, l.point.y, 0.0))}

    val targetLmSide3D = targetLmSide.map{l => new Landmark[_3D](l.id, Point3D(0.0, l.point.y, l.point.x))}

    val modelLmFront = modelLm.filter{l =>
      val filtered = targetLmFront.filter(_.id == l.id)
      filtered.nonEmpty
    }.sortBy(l => l.id)

    val modelLmSide = modelLm.filter{l =>
      val filtered = targetLmSide.filter(_.id == l.id)
      filtered.nonEmpty
    }.sortBy(l => l.id)

    val littleNoiseFront = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * 0.005)
    littleNoiseFront.cov.data(8) = Double.MaxValue

    val referenceMesh = model.referenceMesh
    val referencePIDFront = modelLmFront.map{pt => referenceMesh.pointSet.findClosestPoint(pt.point).id}

    val rigidTransFront = LandmarkRegistration.rigid3DLandmarkRegistration(targetLmFront3D, modelLmFront, computeCentreOfMass(model.referenceMesh))
    val targetFrontTransformed = targetLmFront3D.map{t => t.transform(rigidTransFront).point}
    val regressionDataFront = for ((refPointId, targetPoint) <- referencePIDFront zip targetFrontTransformed) yield {
      (refPointId, targetPoint, littleNoiseFront)
    }

    val posteriorFront = model.posterior(regressionDataFront.toIndexedSeq)

    val littleNoiseSide = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * 0.001)
    littleNoiseSide.cov.data(0) = Double.MaxValue

    val posteriorLmSide = modelLmSide.map{lm => new Landmark[_3D](lm.id, posteriorFront.referenceMesh.pointSet.point(model.referenceMesh.pointSet.findClosestPoint(lm.point).id))}
    val referencePIDSide = posteriorLmSide.map{pt => posteriorFront.referenceMesh.pointSet.findClosestPoint(pt.point).id}

    val rigidTransSide = LandmarkRegistration.rigid3DLandmarkRegistration(targetLmSide3D, posteriorLmSide, computeCentreOfMass(posteriorFront.referenceMesh))
    val targetSideTransformed = targetLmSide3D.map{t => t.transform(rigidTransSide).point}
    val regressionDataSide = for ((refPointId, targetPoint) <- referencePIDSide zip targetSideTransformed) yield {
      (refPointId, targetPoint, littleNoiseSide)
    }

    val posteriorSide = posteriorFront.posterior(regressionDataSide.toIndexedSeq)

    posteriorSide
    //posteriorFront
  }

  def meshArmClip(mesh: TriangleMesh[_3D], meshLm: IndexedSeq[Landmark[_3D]]): TriangleMesh[_3D] = {
    val pid: IndexedSeq[Int] = Source.fromFile("data/mpi-training/armid.txt").getLines().map{id => id.toInt}.toIndexedSeq
    val points = pid.map{id => mesh.pointSet.point(PointId(id))}
    mesh.operations.clip(p => {points.contains(p)})
  }

  def meshFingerClip(mesh: TriangleMesh[_3D], meshLm: IndexedSeq[Landmark[_3D]]): TriangleMesh[_3D] = {
    val radialRt = mesh.pointSet.findClosestPoint(meshLm.filter(p => p.id.contains("radial-styloid.rt")).head.point).point.toVector
    val radialLt = mesh.pointSet.findClosestPoint(meshLm.filter(p => p.id.contains("radial-styloid.lt")).head.point).point.toVector
    val fingertipRt = mesh.pointSet.findClosestPoint(meshLm.filter(p => p.id.contains("fingertip.rt")).head.point).point.toVector
    val fingertipLt = mesh.pointSet.findClosestPoint(meshLm.filter(p => p.id.contains("fingertip.lt")).head.point).point.toVector

    val pointsRadialRt = mesh.pointSet.findNClosestPoints(radialRt.toPoint, 600).map{p => p.point}
    val pointsRadialLt = mesh.pointSet.findNClosestPoints(radialLt.toPoint, 600).map{p => p.point}
    val meshCut = mesh.operations.clip(p => {pointsRadialRt.contains(p) || pointsRadialLt.contains(p)})

    val pointsFingerRt = meshCut.pointSet.findNClosestPoints(fingertipRt.toPoint, 281).map{p => p.point}
    val pointsFingerLt = meshCut.pointSet.findNClosestPoints(fingertipLt.toPoint, 281).map{p => p.point}
    meshCut.operations.clip(p => {pointsFingerRt.contains(p) || pointsFingerLt.contains(p)})
  }

  def get2DPoints(mesh: TriangleMesh[_3D], pointIDs: IndexedSeq[PointId], bearingAngle: Float, orientation: orientationEnum): IndexedSeq[Point[_2D]] = {
    // Center the volume
    val centre = mesh.boundingBox.origin.toVector + mesh.boundingBox.extent*0.5 //computeCentreOfMass(mesh).toVector
    //instead of creating the volume do this. You get: width, length and height of the mesh bounding box
    val coords3D = IntVector((mesh.boundingBox.oppositeCorner(0) - mesh.boundingBox.origin(0)).round.toInt,
      (mesh.boundingBox.oppositeCorner(1) - mesh.boundingBox.origin(1)).round.toInt,
      (mesh.boundingBox.oppositeCorner(2) - mesh.boundingBox.origin(2)).round.toInt
    )

    //then do
    val firstDim = coords3D(0)
    val secondDim = coords3D(2)

    val pointConverter: imageParameters = imageParameters.iniImage.copy(
      // Pitch, yaw, roll
      view = ViewParameter(centre, math.toRadians(0),math.toRadians(0),math.toRadians(bearingAngle)),
      camera = imageParameters.iniImage.camera.copy(orthographic = true, sensorSize = Vector(secondDim, secondDim)),
      imageSize = imageSize(firstDim, secondDim)
    )
    val targetLms: IndexedSeq[Point[_3D]] = pointIDs.map{pid => mesh.pointSet.point(pid)}
    val target2DPoints = targetLms.map{lm =>
      val imagePlanePoint = pointConverter.renderTransform(lm)
      var pointDim = 0.0

      if (orientation == orientationEnum.FRONT){
        pointDim = imagePlanePoint.z
      }
      else {
        pointDim = imagePlanePoint.x
      }

      Point(pointDim,imagePlanePoint.y)
    }
    target2DPoints
  }

  /**
    * Calculates an image scaling factor based on camera parameters. What if object was certain distance from camera
    *
    * Parameters:
    *    -   `realHeight` Real height of the object (mm)
    *    -   `imageHeight` Height of the image (pixels)
    *    -   `objectHeight` Height of the object (pixels)
    *    -   `focalLength` Focal length of camera (mm)
    *    -   `sensorHeight` Height of camera sensor (mm)
    *    -   `subjectDistance` Distance of subject (mm)
    * Returns:
    *    -  Image scaling factor
    */
  def imageScalingFactor(realHeight: Double, imageHeight: Int, objectHeight: Double, focalLength: Double = 3.8, sensorHeight: Double = 8.3, subjectDistance: Double = 1000.0): Double = {
    val scaledObjectHeight = focalLength * realHeight * imageHeight/(sensorHeight * subjectDistance)
    val scalingFactor = scaledObjectHeight/objectHeight
    val dimensionScaler = realHeight/(1000 * scaledObjectHeight)
    scalingFactor * dimensionScaler
  }

  def get3DPoints(mesh: TriangleMesh[_3D], pid: PointId, bearingAngle: Float = 0): Point[_3D] = {
    val centreOfMass = mesh.boundingBox.origin.toVector + mesh.boundingBox.extent*0.5 //computeCenterOfMass(mesh).toVector
    //instead of creating the volume do this. You get: width, length and height of the mesh bounding box
    val coords3D = IntVector((mesh.boundingBox.oppositeCorner(0) - mesh.boundingBox.origin(0)).round.toInt,
      (mesh.boundingBox.oppositeCorner(1) - mesh.boundingBox.origin(1)).round.toInt,
      (mesh.boundingBox.oppositeCorner(2) - mesh.boundingBox.origin(2)).round.toInt
    )

    //then do
    val firstDim = coords3D(0)
    val secondDim = coords3D(2)

    val pointConverter: imageParameters = imageParameters.iniImage.copy(
      // Pitch, yaw, roll
      camera = imageParameters.iniImage.camera.copy(orthographic = true, sensorSize = Vector(firstDim, secondDim)),
      imageSize = imageSize(firstDim, secondDim),
      view = ViewParameter(centreOfMass, math.toRadians(0),math.toRadians(0),math.toRadians(bearingAngle))
    )

    pointConverter.inverseRenderTransform(mesh.pointSet.point(pid))
  }

  def getProjectedPoint(mesh: TriangleMesh[_3D], point: Point[_3D], pixelHeight: Double, realHeight: Double = 1.3): Point[_3D] = {

    val centreOfMass = mesh.boundingBox.origin.toVector + mesh.boundingBox.extent*0.5 //computeCenterOfMass(mesh).toVector
    //instead of creating the volume do this. You get: width, length and height of the mesh bounding box
    val coords3D = IntVector((mesh.boundingBox.oppositeCorner(0) - mesh.boundingBox.origin(0)).round.toInt,
      (mesh.boundingBox.oppositeCorner(1) - mesh.boundingBox.origin(1)).round.toInt,
      (mesh.boundingBox.oppositeCorner(2) - mesh.boundingBox.origin(2)).round.toInt
    )

    //then do
    val firstDim = coords3D(0)
    val secondDim = coords3D(2)

    val meshHeight = Measurement.measurePointHeight(mesh)
    val scaler = meshHeight/realHeight

    val pointConverter: imageParameters = imageParameters.iniImage.copy(
      // Pitch, yaw, roll
      camera = imageParameters.iniImage.camera.copy(orthographic = true, sensorSize = Vector(firstDim, secondDim)),
      imageSize = imageSize(firstDim, secondDim),
      view = ViewParameter(centreOfMass, math.toRadians(0),math.toRadians(0),math.toRadians(0))
    )

    pointConverter.inverseRenderTransform(Vector(point.x * scaler, point.y * scaler, point.z).toPoint)
  }

  def computeCentreOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
    val normFactor = 1.0 / mesh.pointSet.numberOfPoints
    mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
  }

  def augmentFittingModel (model: StatisticalMeshModel, l: Double, s: Double): StatisticalMeshModel = {
    val scalarValuedKernel = GaussianKernel[_3D](s) * l

    case class XmirroredKernel(kernel : PDKernel[_3D]) extends PDKernel[_3D] {
      override def domain = RealSpace[_3D]
      override def k(x: Point[_3D], y: Point[_3D]) = kernel(Point(x(0) * -1f ,x(1), x(2)), y)
    }

    def symmetrizeKernel(kernel : PDKernel[_3D]) : MatrixValuedPDKernel[_3D] = {
      val xmirrored = XmirroredKernel(kernel)
      val k1 = DiagonalKernel(kernel, 3)
      val k2 = DiagonalKernel(xmirrored * -1f, xmirrored, xmirrored)
      k1 + k2
    }

    val gp = GaussianProcess[_3D, Vector[_3D]](symmetrizeKernel(scalarValuedKernel))

    val sampler = RandomMeshSampler3D(
      model.referenceMesh,
      numberOfPoints = 800,
      seed = 0,
    )
    val lowRankGP = LowRankGaussianProcess.approximateGP(
      gp,
      sampler,
      numBasisFunctions = 200,
    )(ThreeDSpace, vectorizer = gp.vectorizer, rand = rng)

    StatisticalMeshModel.augmentModel(model, lowRankGP)
  }

  object orientationEnum extends Enumeration {
    type orientationEnum = Value
    val FRONT, SIDE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      orientationEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
  }

  private case class fittingData(lmFront: IndexedSeq[Landmark[_2D]], lmSide: IndexedSeq[Landmark[_2D]], sex: sexEnum){
    def getLmFront: IndexedSeq[Landmark[_2D]] = lmFront
    def getLmSide: IndexedSeq[Landmark[_2D]] = lmSide
    def getSex: sexEnum = sex
  }

  private case class modelContainer(model: StatisticalMeshModel, modelLM: Seq[Landmark[_3D]]){
    def getModel: StatisticalMeshModel = model
    def getLM: Seq[Landmark[_3D]] = modelLM
  }

  private case class fittingModelContainer(fitData: fittingData, modelCon: modelContainer){
    def getData: fittingData = fitData
    def getModel: modelContainer = modelCon
  }

}