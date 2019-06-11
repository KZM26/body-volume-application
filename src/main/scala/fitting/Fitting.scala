package fitting

import java.io.File

import javax.imageio.ImageIO

import scala.io.Source
import breeze.linalg.{DenseMatrix, DenseVector}
import build.Build.rng
import scalismo.common._
import scalismo.faces.io.{PixelImageIO, TLMSLandmarksIO}
import scalismo.geometry._
import scalismo.io.{LandmarkIO, StatismoIO}
import scalismo.mesh.TriangleMesh
import scalismo.registration.{LandmarkRegistration, RigidTransformation, RotationTransform, TranslationTransform}
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.evaluators.ProductEvaluator
import scalismo.sampling.proposals.MixtureProposal
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator, TransitionProbability}
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.ui.api.{ScalismoUI, ShowInScene}
import scalismo.utils.Memoize
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
          fittingTestLandmark()
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
      val (samples, posteriorEvaluator) = fitImage(model, modelLm, l, targetLmSide(targetLmFront.indexOf(l)), null)
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

  def fittingTestLandmark(): Unit = {
    //val ui = ScalismoUI()

    val model = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get
    val scalarValuedKernel = GaussianKernel[_3D](70) * 100.0

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
    val tlmsLandmarks = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("tlms")}
    val imageFiles = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("png")}

    val imgLmFront: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      // Scale image. Scale to correct distance and coordinate system
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = Vector(l.point.x * scaleFactor, (image.getHeight - l.point.y*0) * scaleFactor).toPoint
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
        val newPoint = Vector(l.point.x * scaleFactor, (image.getHeight - l.point.y*0) * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront
    }
    /*val targetLmViews = ui.show(targetGroup, targetLm, "targetLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.RED)
*/
    println("Data loaded. Starting fitting")

    val bestFitsPosterior = targetLmFront.indices.map{i =>
      val posterior = landmarkPosterior(augModel, modelLm, targetLmFront(i), targetLmSide(i))
      posterior.mean
    }/*
    val resultGroup = ui.createGroup("Results")
    bestFitsPosterior.indices.map{i=>
      ui.show(resultGroup, bestFitsPosterior(i), i.toString)
    }*/

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
    }

    val modelLmSide = modelLm.filter{l =>
      val filtered = targetLmSide.filter(_.id == l.id)
      filtered.nonEmpty
    }

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
  }

  def fitImage(model: StatisticalMeshModel, modelLm: Seq[Landmark[_3D]], targetLmFront: Seq[Landmark[_2D]], targetLmSide: Seq[Landmark[_2D]],
               modelView: ShowInScene.ShowInSceneStatisticalMeshModel.View): (IndexedSeq[Fitting.Sample], ProductEvaluator[Fitting.Sample]) = {

    // Fit front image first
    val (samplesFront, posteriorEval) = fitImage(model, modelLm, targetLmFront, modelView)
    val bestSample = samplesFront.maxBy(posteriorEval.logValue)
    val bestFit = model.instance(bestSample.parameters.modelCoefficients).transform(bestSample.poseTransformation)

    val bestFitHeight = Measurement.measurePointHeight(bestFit)

    val waistAntID = model.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.anterior").head.point).id
    val waistPostID = model.referenceMesh.pointSet.findClosestPoint(modelLm.filter(p => p.id == "waist.posterior").head.point).id
    val bestFitWC = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1000 // Convert to mm

    val bodyFat = Measurement.getBodyFatPercentage(bestFit, bestFitHeight, 64, 24, Measurement.sexEnum.MALE)

    println(bestFitHeight)
    println(bestFitWC)
    println(bodyFat)

    // Fit side image next using the bestFit as the initial sample
    val modelLmIDs =  modelLm.map{l => bestFit.pointSet.findClosestPoint(l.point).id}.toIndexedSeq
    val targetPoints = targetLmSide.map{l => l.point}.toIndexedSeq

    // Add some noise to target landmarks.
    // Model as normal distribution
    val landmarkNoiseVariance = 9.0
    val uncertainty = MultivariateNormalDistribution(
      DenseVector.zeros[Double](2),
      DenseMatrix.eye[Double](2) * landmarkNoiseVariance
    )

    val correspondences = modelLmIDs.zip(targetPoints).map{modelIdWithTargetPoint =>
      val (modelId, targetPoint) =  modelIdWithTargetPoint
      (modelId, targetPoint, uncertainty)
    }

    val likelihoodEvaluator = CachedEvaluator(CorrespondenceEvaluator(model, correspondences, orientationEnum.SIDE))
    val priorEvaluator = CachedEvaluator(PriorEvaluator(model))

    val posteriorEvaluator = ProductEvaluator(priorEvaluator, likelihoodEvaluator)

    // Set proposal update mixture
    val shapeUpdateProposal = ShapeUpdateProposal(model.rank, 0.1)
    val rotationUpdateProposal = RotationUpdateProposal(0.01)
    val translationUpdateProposal = TranslationUpdateProposal(1.0)
    val generator = MixtureProposal.fromProposalsWithTransition(
      (0.7, shapeUpdateProposal),
      (0.3, rotationUpdateProposal),
      (0.0, translationUpdateProposal)
    )

    // Set initialisation values
    val initialParameters = Parameters(Vector(0, 0, 0), (0.0, 0.0, 0.0), DenseVector.zeros[Double](model.rank))
    val initialSample = Sample("initial", initialParameters, computeCentreOfMass(bestFit))

    // Set up chain and obtain iterator
    val chain = MetropolisHastings(generator, posteriorEvaluator)
    val logger = new Logger()
    val mhIterator = chain.iterator(initialSample, logger)

    val samplingIterator = for((sample, iteration) <- mhIterator.zipWithIndex) yield {
      // Visualisation code
      //println("iteration " + iteration)
      if ((modelView != null) && (iteration % 500 == 0)) {
        modelView.shapeModelTransformationView.shapeTransformationView.coefficients = sample.parameters.modelCoefficients
        modelView.shapeModelTransformationView.poseTransformationView.transformation = sample.poseTransformation
      }

      sample
    }

    val samples = samplingIterator.slice(1000, 11000).toIndexedSeq
    println(logger.acceptanceRatios())
    (samples, posteriorEvaluator)
  }

  def fitImage(model: StatisticalMeshModel, modelLm: Seq[Landmark[_3D]], targetLm: Seq[Landmark[_2D]],
               modelView: ShowInScene.ShowInSceneStatisticalMeshModel.View): (IndexedSeq[Fitting.Sample], ProductEvaluator[Fitting.Sample]) = {

    val modelLmIDs =  modelLm.map{l => model.referenceMesh.pointSet.findClosestPoint(l.point).id}.toIndexedSeq
    val targetPoints = targetLm.map{l => l.point}.toIndexedSeq

    // Add some noise to target landmarks.
    // Model as normal distribution
    val landmarkNoiseVariance = 9.0
    val uncertainty = MultivariateNormalDistribution(
      DenseVector.zeros[Double](2),
      DenseMatrix.eye[Double](2) * landmarkNoiseVariance
    )

    val correspondences = modelLmIDs.zip(targetPoints).map{modelIdWithTargetPoint =>
      val (modelId, targetPoint) =  modelIdWithTargetPoint
      (modelId, targetPoint, uncertainty)
    }

    val likelihoodEvaluator = CachedEvaluator(CorrespondenceEvaluator(model, correspondences, orientationEnum.FRONT))
    val priorEvaluator = CachedEvaluator(PriorEvaluator(model))

    val posteriorEvaluator = ProductEvaluator(priorEvaluator, likelihoodEvaluator)

    // Set proposal update mixture
    val shapeUpdateProposal = ShapeUpdateProposal(model.rank, 0.1)
    val rotationUpdateProposal = RotationUpdateProposal(0.01)
    val translationUpdateProposal = TranslationUpdateProposal(1.0)
    val generator = MixtureProposal.fromProposalsWithTransition(
      (0.7, shapeUpdateProposal),
      (0.3, rotationUpdateProposal),
      (0.0, translationUpdateProposal)
    )

    // Set initialisation values
    val initialParameters = Parameters(Vector(0, 0, 0), (0.0, 0.0, 0.0), DenseVector.zeros[Double](model.rank))
    val initialSample = Sample("initial", initialParameters, computeCentreOfMass(model.mean))

    // Set up chain and obtain iterator
    val chain = MetropolisHastings(generator, posteriorEvaluator)
    val logger = new Logger()
    val mhIterator = chain.iterator(initialSample, logger)

    val samplingIterator = for((sample, iteration) <- mhIterator.zipWithIndex) yield {
      // Visualisation code
      //println("iteration " + iteration)
      if ((modelView != null) && (iteration % 500 == 0)) {
        modelView.shapeModelTransformationView.shapeTransformationView.coefficients = sample.parameters.modelCoefficients
        modelView.shapeModelTransformationView.poseTransformationView.transformation = sample.poseTransformation
      }

      sample
    }

    val samples = samplingIterator.slice(1000, 11000).toIndexedSeq
    println(logger.acceptanceRatios())
    (samples, posteriorEvaluator)
  }

  def computeCentreOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
    val normFactor = 1.0 / mesh.pointSet.numberOfPoints
    mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
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

  // 2D image marginalisation
  private def marginalizeModelForCorrespondences(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_2D], MultivariateNormalDistribution)]):
  (StatisticalMeshModel, Seq[(PointId, Point[_2D], MultivariateNormalDistribution)]) = {

    val (modelIds, _, _) = correspondences.unzip3
    val marginalizedModel = model.marginal(modelIds.toIndexedSeq)
    val newCorrespondences = correspondences.map{ idWithTargetPoint =>
      val (id, targetPoint, uncertainty) = idWithTargetPoint
      val modelPoint = model.referenceMesh.pointSet.point(id)
      val newId = marginalizedModel.referenceMesh.pointSet.findClosestPoint(modelPoint).id
      (newId, targetPoint, uncertainty)
    }
    (marginalizedModel, newCorrespondences)
  }

  // Case classes
  case class Parameters(translationParameters: Vector[_3D], rotationParameters: (Double, Double, Double), modelCoefficients: DenseVector[Double])

  case class Sample(generatedBy: String, parameters: Parameters, rotationCenter: Point[_3D]) {
    def poseTransformation : RigidTransformation[_3D] = {

      val translation = TranslationTransform(parameters.translationParameters)
      val rotation = RotationTransform(
        parameters.rotationParameters._1,
        parameters.rotationParameters._2,
        parameters.rotationParameters._3,
        rotationCenter
      )
      RigidTransformation(translation, rotation)
    }
  }

  private case class PriorEvaluator(model: StatisticalMeshModel) extends DistributionEvaluator[Sample] {

    val translationPrior = breeze.stats.distributions.Gaussian(0.0, 5.0)
    val rotationPrior = breeze.stats.distributions.Gaussian(0, 0.1)

    override def logValue(sample: Sample): Double = {
      model.gp.logpdf(sample.parameters.modelCoefficients) +
        translationPrior.logPdf(sample.parameters.translationParameters.x) +
        translationPrior.logPdf(sample.parameters.translationParameters.y) +
        translationPrior.logPdf(sample.parameters.translationParameters.z) +
        rotationPrior.logPdf(sample.parameters.rotationParameters._1) +
        rotationPrior.logPdf(sample.parameters.rotationParameters._2) +
        rotationPrior.logPdf(sample.parameters.rotationParameters._3)
    }
  }

  // 3D mesh vs 2D image
  private case class SimpleCorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_2D], MultivariateNormalDistribution)])
    extends DistributionEvaluator[Sample] {

    override def logValue(sample: Sample): Double = {

      val currModelInstance = model.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = correspondences.map{correspondence => {
        val (id, targetPoint, uncertainty) = correspondence
        val modelInstancePoint = currModelInstance.pointSet.point(id)
        val truncInstance: Vector[_2D] = Vector(modelInstancePoint.y, modelInstancePoint.z)
        val observedDeformation = targetPoint - truncInstance

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }}


      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }

  // 3D mesh vs 2D image
  private case class CorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_2D], MultivariateNormalDistribution)], orientation: orientationEnum)
    extends DistributionEvaluator[Sample] {

    val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

    override def logValue(sample: Sample): Double = {

      val currModelInstance = marginalizedModel.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = newCorrespondences.map{correspondence =>
        val (id, targetPoint, uncertainty) = correspondence
        val pointIDs: IndexedSeq[PointId] = IndexedSeq[PointId](id, PointId(0))
        val projectedModelInstancePoint = get2DPoints(currModelInstance, pointIDs, 0, orientation).head

        val observedDeformation = targetPoint - projectedModelInstancePoint

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }

      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }

  private case class CachedEvaluator[A](evaluator: DistributionEvaluator[A]) extends DistributionEvaluator[A] {
    val memoizedLogValue = Memoize(evaluator.logValue, 10)

    override def logValue(sample: A): Double = {
      memoizedLogValue(sample)
    }
  }

  private case class ShapeUpdateProposal(paramVectorSize: Int, stddev: Double)
    extends ProposalGenerator[Sample] with TransitionProbability[Sample] {

    val perturbationDistr = new MultivariateNormalDistribution(
      DenseVector.zeros(paramVectorSize),
      DenseMatrix.eye[Double](paramVectorSize) * stddev * stddev
    )


    override def propose(sample: Sample): Sample = {
      val perturbation = perturbationDistr.sample()
      val newParameters = sample.parameters.copy(modelCoefficients = sample.parameters.modelCoefficients + perturbationDistr.sample)
      sample.copy(generatedBy = s"ShapeUpdateProposal ($stddev)", parameters = newParameters)
    }

    override def logTransitionProbability(from: Sample, to: Sample) = {
      val residual = to.parameters.modelCoefficients - from.parameters.modelCoefficients
      perturbationDistr.logpdf(residual)
    }
  }

  private case class RotationUpdateProposal(stddev: Double) extends
    ProposalGenerator[Sample] with TransitionProbability[Sample] {

    val perturbationDistr = new MultivariateNormalDistribution(
      DenseVector.zeros[Double](3),
      DenseMatrix.eye[Double](3) * stddev * stddev)

    def propose(sample: Sample): Sample= {
      val perturbation = perturbationDistr.sample
      val newRotationParameters = (
        sample.parameters.rotationParameters._1 + perturbation(0),
        sample.parameters.rotationParameters._2 + perturbation(1),
        sample.parameters.rotationParameters._3 + perturbation(2)
      )
      val newParameters = sample.parameters.copy(rotationParameters = newRotationParameters)
      sample.copy(generatedBy = s"RotationUpdateProposal ($stddev)", parameters = newParameters)
    }
    override def logTransitionProbability(from: Sample, to: Sample) = {
      val residual = DenseVector(
        to.parameters.rotationParameters._1 - from.parameters.rotationParameters._1,
        to.parameters.rotationParameters._2 - from.parameters.rotationParameters._2,
        to.parameters.rotationParameters._3 - from.parameters.rotationParameters._3
      )
      perturbationDistr.logpdf(residual)
    }
  }

  private case class TranslationUpdateProposal(stddev: Double) extends
    ProposalGenerator[Sample] with TransitionProbability[Sample] {

    val perturbationDistr = new MultivariateNormalDistribution( DenseVector.zeros(3),
      DenseMatrix.eye[Double](3) * stddev * stddev)

    def propose(sample: Sample): Sample= {
      val newTranslationParameters = sample.parameters.translationParameters + Vector.fromBreezeVector(perturbationDistr.sample())
      val newParameters = sample.parameters.copy(translationParameters = newTranslationParameters)
      sample.copy(generatedBy = s"TranlationUpdateProposal ($stddev)", parameters = newParameters)
    }

    override def logTransitionProbability(from: Sample, to: Sample) = {
      val residual = to.parameters.translationParameters - from.parameters.translationParameters
      perturbationDistr.logpdf(residual.toBreezeVector)
    }
  }

  object orientationEnum extends Enumeration {
    type orientationEnum = Value
    val FRONT, SIDE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      orientationEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
  }

}