package fitting

import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector}
import scalismo.common.PointId
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.geometry._
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO}
import scalismo.mesh.TriangleMesh
import scalismo.registration.{RigidTransformation, RigidTransformationSpace, RotationTransform, TranslationTransform}
import scalismo.sampling.algorithms.MetropolisHastings
import scalismo.sampling.evaluators.ProductEvaluator
import scalismo.sampling.proposals.MixtureProposal
import scalismo.sampling.loggers.AcceptRejectLogger
import scalismo.sampling.{DistributionEvaluator, ProposalGenerator, TransitionProbability}
import scalismo.statisticalmodel.{MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.ui.api.{Group, ScalismoUI, ShowInScene}
import scalismo.utils.Memoize

object Fitting {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)
  println("Initialising Scalismo")
  scalismo.initialize()
  println("Done")

  def start(): Unit = {
    val fittingConfig = List("Start measurements (f)", "Experiments (e)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(fittingConfig.mkString("\n")).toLowerCase()
      input match {

        case "s" => // Start fitting
          //TODO

        case "e" => // Start experiments
          fittingTest()

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

  def fittingTest(): Unit = {
    // Load SSM and landmarks defined on reference
    val ui = ScalismoUI()

    val model = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get
    val modelGroup = ui.createGroup("model")
    val modelView = ui.show(modelGroup, model, "model")
    modelView.meshView.opacity = 0.5

    val modelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/minimal.json")).get
    val modelLmViews = ui.show(modelGroup, modelLm, "modelLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.BLUE)

    val targetGroup = ui.createGroup("target")

    val targetLm = TLMSLandmarksIO.read2D(new File("data/image-landmarks/000.tlms")).get.map{TLMSLandmark => TLMSLandmark.toLandmark}
    /*val targetLmViews = ui.show(targetGroup, targetLm, "targetLandmarks")
    modelLmViews.foreach(lmView => lmView.color = java.awt.Color.RED)
*/
    println("Data loaded. Starting fitting")
    val (samples, posteriorEvaluator) = fitImage(model, modelLm, targetLm, modelView)

    val bestSample = samples.maxBy(posteriorEvaluator.logValue)
    val bestFit = model.instance(bestSample.parameters.modelCoefficients).transform(bestSample.poseTransformation)
    val resultGroup = ui.createGroup("result")
    ui.show(resultGroup, bestFit, "best fit")

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

    val likelihoodEvaluator = CachedEvaluator(CorrespondenceEvaluator(model, correspondences))
    val priorEvaluator = CachedEvaluator(PriorEvaluator(model))

    val posteriorEvaluator = ProductEvaluator(priorEvaluator, likelihoodEvaluator)

    // Set proposal update mixture
    val shapeUpdateProposal = ShapeUpdateProposal(model.rank, 0.1)
    val rotationUpdateProposal = RotationUpdateProposal(0.01)
    val translationUpdateProposal = TranslationUpdateProposal(1.0)
    val generator = MixtureProposal.fromProposalsWithTransition(
      (0.6, shapeUpdateProposal),
      (0.2, rotationUpdateProposal),
      (0.2, translationUpdateProposal)
    )

    // Set initialisation values
    val initialParameters = Parameters(Vector(0, 0, 0), (0.0, 0.0, 0.0), DenseVector.zeros[Double](model.rank))
    val initialSample = Sample("initial", initialParameters, computeCenterOfMass(model.mean))

    // Set up chain and obtain iterator
    val chain = MetropolisHastings(generator, posteriorEvaluator)
    val logger = new Logger()
    val mhIterator = chain.iterator(initialSample, logger)

    val samplingIterator = for((sample, iteration) <- mhIterator.zipWithIndex) yield {
      // Visualisation code
      println("iteration " + iteration)
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

  def computeCenterOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
    val normFactor = 1.0 / mesh.pointSet.numberOfPoints
    mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
  }

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

  private case class SimpleCorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_2D], MultivariateNormalDistribution)])
    extends DistributionEvaluator[Sample] {

    override def logValue(sample: Sample): Double = {

      val currModelInstance = model.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = correspondences.map{correspondence => {
        val (id, targetPoint, uncertainty) = correspondence
        val modelInstancePoint = currModelInstance.pointSet.point(id)
        val truncInstance: Vector[_2D] = Vector(modelInstancePoint.x, modelInstancePoint.y)
        //val augTarget: Vector[_3D] = Vector(targetPoint.x, targetPoint.y, modelInstancePoint.z)
        val observedDeformation = targetPoint - truncInstance
        //val observedDeformation = augTarget.toPoint - modelInstancePoint

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }}


      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }

  private case class CorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_2D], MultivariateNormalDistribution)])
    extends DistributionEvaluator[Sample] {

    val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

    override def logValue(sample: Sample): Double = {

      val currModelInstance = marginalizedModel.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = newCorrespondences.map{correspondence =>
        val (id, targetPoint, uncertainty) = correspondence
        val modelInstancePoint = currModelInstance.pointSet.point(id)
        val truncInstance: Vector[_2D] = Vector(modelInstancePoint.x, modelInstancePoint.y)
        //val augTarget: Vector[_3D] = Vector(targetPoint.x, targetPoint.y, modelInstancePoint.z)
        val observedDeformation = targetPoint - truncInstance
        //val observedDeformation = augTarget.toPoint - modelInstancePoint

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }


      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }
/*
  private case class SimpleCorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_3D], MultivariateNormalDistribution)])
    extends DistributionEvaluator[Sample] {

    override def logValue(sample: Sample): Double = {

      val currModelInstance = model.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = correspondences.map{correspondence => {
        val (id, targetPoint, uncertainty) = correspondence
        val modelInstancePoint = currModelInstance.pointSet.point(id)
        val observedDeformation = targetPoint - modelInstancePoint

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }}


      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }

  private case class CorrespondenceEvaluator(model: StatisticalMeshModel, correspondences: Seq[(PointId, Point[_3D], MultivariateNormalDistribution)])
    extends DistributionEvaluator[Sample] {

    val (marginalizedModel, newCorrespondences) = marginalizeModelForCorrespondences(model, correspondences)

    override def logValue(sample: Sample): Double = {

      val currModelInstance = marginalizedModel.instance(sample.parameters.modelCoefficients).transform(sample.poseTransformation)

      val likelihoods = newCorrespondences.map{correspondence =>
        val (id, targetPoint, uncertainty) = correspondence
        val modelInstancePoint = currModelInstance.pointSet.point(id)
        val observedDeformation = targetPoint - modelInstancePoint

        uncertainty.logpdf(observedDeformation.toBreezeVector)
      }


      val loglikelihood = likelihoods.sum
      loglikelihood
    }
  }
*/
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

}