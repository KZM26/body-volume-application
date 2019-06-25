package fitting

import java.io.File

import javax.imageio.ImageIO

import scala.io.Source
import breeze.linalg.{DenseMatrix, DenseVector}

import scalismo.common._
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.geometry._
import scalismo.io.{LandmarkIO, StatismoIO}
import scalismo.mesh.TriangleMesh
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.ui.api.ScalismoUI
import measurement.Measurement
import fitting.Fitting.orientationEnum.orientationEnum
import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.faces.parameters.ViewParameter
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.kernels.{DiagonalKernel, GaussianKernel, MatrixValuedPDKernel, PDKernel}
import scalismo.numerics.RandomMeshSampler3D

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
          val l = IndexedSeq(1.5)
          for (i <- l) {
            fittingTestLandmark(i)
            println(i.toString)
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

  def fittingTest(): Unit = {
    // Load SSM and landmarks defined on reference
    val ui = ScalismoUI()

    val model = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get
    /*val modelGroup = ui.createGroup("model")
    val modelView = ui.show(modelGroup, model, "model")
    modelView.meshView.opacity = 0.5*/

    val modelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/spring-minimal.json")).get
    //val modelLmViews = ui.show(modelGroup, modelLm, "modelLandmarks")
    //modelLmViews.foreach(lmView => lmView.color = java.awt.Color.BLUE)

    //val targetGroup = ui.createGroup("target")
    val realHeight: IndexedSeq[Double] = Source.fromFile("data/image-landmarks/height.txt").getLines().toIndexedSeq.map{h => h.toDouble}
    val tlmsLandmarks = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("tlms")}
    val imageFiles = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("png")}

    val imgLmFront: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      // Scale & flip image. Scale to correct distance and flip since shape model upside down and make fitting quicker
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = Vector(l.point.x * scaleFactor, (image.getHeight - l.point.y)  * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront
    }

    val sideImages = imageFiles.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val imgLmSide: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val targetLmSide: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmSide.indices.map{i =>
      val lmHeightSide = imgLmSide(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmSide(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = sideImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightSide)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmSide(i).map{l =>
        val newPoint = Vector(l.point.x * scaleFactor, (image.getHeight - l.point.y) * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront
    }

    /*val targetLmViews = ui.show(targetGroup, targetLm, "targetLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.RED)
*/
    println("Data loaded. Starting fitting")
    val bestFits = targetLmFront.map{l =>
      val (samples, posteriorEvaluator) = Sampling.fitImage(model, modelLm, l, targetLmSide(targetLmFront.indexOf(l)), null)
      val bestSample = samples.maxBy(posteriorEvaluator.logValue)
      val bestFit = model.instance(bestSample.parameters.modelCoefficients).transform(bestSample.poseTransformation)
      bestFit
    }.toIndexedSeq

    /*val (samples, posteriorEvaluator) = fitImage(model, modelLm, targetLmFront, targetLmSide, modelView)

    val bestSample = samples.maxBy(posteriorEvaluator.logValue)
    val bestFit = model.instance(bestSample.parameters.modelCoefficients).transform(bestSample.poseTransformation)
    val resultGroup = ui.createGroup("result")
    ui.show(resultGroup, bestFit, "best fit")*/
    val resultGroup = ui.createGroup("result")
    bestFits.indices.map{i=>
      ui.show(resultGroup, bestFits(i), i.toString)
    }

    val waistAntID = model.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.anterior").head.point).id
    val waistPostID = model.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.posterior").head.point).id

    val bestFitHeight = bestFits.map{bestFit => Measurement.measurePointHeight(bestFit)}
    val bestFitWC = bestFits.map{bestFit => Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1000}
    val bodyVol = bestFits.map{bestFit => Measurement.getMeshVolume(bestFit) * 1e+3}

    println("Height")
    println(bestFitHeight)
    println("WC")
    println(bestFitWC)
    println("Vol")
    println(bodyVol)

  }

  def fittingTestLandmark(l: Double): Unit = {
    val model = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get
    val scalarValuedKernel = GaussianKernel[_3D](0.5) * l

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

    val augModel = StatisticalMeshModel.augmentModel(model, lowRankGP)

    val modelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/spring-minimal.json")).get
    //val modelLmViews = ui.show(modelGroup, modelLm, "modelLandmarks")
    //modelLmViews.foreach(lmView => lmView.color = java.awt.Color.BLUE)

    //val targetGroup = ui.createGroup("target")
    val realHeight: IndexedSeq[Double] = Source.fromFile("data/image-landmarks/height.txt").getLines().toIndexedSeq.map{h => h.toDouble}
    val tlmsLandmarks = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("tlms")}.sortBy{f => f.getName}
    val imageFiles = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("png")}.sortBy{f => f.getName}

    val imgLmFront: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
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

    val targetLmFrontFiltered = targetLmFront.map{lmseq =>
      lmseq.filter{p => !p.id.contains("acromion")}.sortBy(f => f.id)

    }
    val targetLmSideFiltered = targetLmSide.map{lmseq =>
      lmseq.filter{p => !p.id.contains("metatarsal")}.sortBy(f => f.id)
    }
    /*val targetLmViews = ui.show(targetGroup, targetLm, "targetLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.RED)
*/
    println("Data loaded. Starting fitting")

    val bestFitsPosterior = targetLmFront.indices.map{i =>
      val posterior = landmarkPosterior(augModel, modelLm, targetLmFrontFiltered(i), targetLmSide(i))
      posterior.mean
    }
    val ui = ScalismoUI()
    val resultGroup = ui.createGroup("Results")
    bestFitsPosterior.indices.map{i=>
      ui.show(resultGroup, bestFitsPosterior(i), i.toString)
    }

    val waistAntID = augModel.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.anterior").head.point).id
    val waistPostID = augModel.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.posterior").head.point).id

    val bestFitHeightPosterior = bestFitsPosterior.map{bestFit => Measurement.measurePointHeight(bestFit)}
    val bestFitWCPosterior = bestFitsPosterior.map{bestFit => Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1000}
    val bodyVolPosterior = bestFitsPosterior.map{bestFit => Measurement.getMeshVolume(bestFit) * 1e+3}

    println("Height Posterior")
    println(bestFitHeightPosterior)
    println("WC Posterior")
    println(bestFitWCPosterior)
    println("Vol Posterior")
    println(bodyVolPosterior)
  }

  def landmarkPosterior(model: StatisticalMeshModel, modelLm: Seq[Landmark[_3D]], targetLmFront: Seq[Landmark[_2D]], targetLmSide: Seq[Landmark[_2D]]): StatisticalMeshModel = {

    val cameraInfo = imageParameters.huaweiP8.camera.copy(orthographic = true, focalLength = 3.8, sensorSize = Vector(8.3, 8.3))
    //val imageInfo = imageSize(1585, 970)
    val imageInfo = imageSize(970, 70)
    val viewInfoAP = ViewParameter(Vector(0f, 0f, 1000f), 0f, 0f, 0f)
    val imageConverterAP: imageParameters = imageParameters.huaweiP8.copy(
      camera = cameraInfo,
      imageSize = imageInfo,
      view = viewInfoAP
    )
    val viewInfoLat = ViewParameter(Vector(1000f, 0f, 0f), 0f, 0f, 0f)
    val imageConverterLat: imageParameters = imageParameters.huaweiP8.copy(
      camera = cameraInfo,
      imageSize = imageInfo,
      view = viewInfoLat
    )

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

    val littleNoiseFront = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * 0.5)
    littleNoiseFront.cov.data(8) = 100000000000000.0

    val referenceMesh = model.referenceMesh
    val referencePIDFront = modelLmFront.map{pt => referenceMesh.pointSet.findClosestPoint(pt.point).id}

    val rigidTransFront = LandmarkRegistration.rigid3DLandmarkRegistration(targetLmFront3D, modelLmFront, computeCentreOfMass(model.referenceMesh))
    val targetFrontTransformed = targetLmFront3D.map{t => t.transform(rigidTransFront).point}
    val regressionDataFront = for ((refPointId, targetPoint) <- referencePIDFront zip targetFrontTransformed) yield {
      (refPointId, targetPoint, littleNoiseFront)
    }

    val posteriorFront = model.posterior(regressionDataFront.toIndexedSeq)

    val littleNoiseSide = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * 0.5)
    littleNoiseSide.cov.data(0) = 100000000000000.0

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

  object orientationEnum extends Enumeration {
    type orientationEnum = Value
    val FRONT, SIDE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      orientationEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
  }

}