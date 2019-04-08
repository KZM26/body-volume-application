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
import scalismo.ui.api.ScalismoUI
import scalismo.utils.Memoize

object Fitting {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)
  scalismo.initialize()

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
    val model = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get
    val modelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/minimal.json")).get
    val targetLm = TLMSLandmarksIO.read2D(new File("data/fit-landmarks/000.tlms")).get

    // Get model point IDs and target points
    val modelLmIDs = modelLm.map{l =>
      model.mean.pointSet.pointId(l.point).get
    }
    val targetPoints = targetLm.map{l =>
      l.point
    }

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


  }

  def fitImage(model: StatisticalMeshModel): Unit = {

  }
}