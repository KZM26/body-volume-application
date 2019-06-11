package validation

import java.io._
import java.util._

import build.Build
import scalismo.geometry._3D
import scalismo.io._
import scalismo.statisticalmodel.dataset._
import scalismo.statisticalmodel._
import scalismo.mesh._
import tools.Utils

object Specificity {
  
  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def specificity(datasetPath: String, distance: String, nb: Int, outputPath: String, landmarkPath: String) {
    scalismo.initialize()

    // Build GPMM from DataCollection
    val training = new File(datasetPath.concat("training/")).listFiles
    val testing = new File(datasetPath.concat("testing/")).listFiles

    val datasetTraining = training.map{f => MeshIO.readMesh(f).get}.toIndexedSeq

    val dc : DataCollection = DataCollection.fromMeshSequence(datasetTraining.head, datasetTraining.tail)._1.get

    val gpModel : StatisticalMeshModel = Build.buildGP(dc)

    // Varying GP rank
    val limited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModel, datasetTraining.size)

    // Specificity
    val datasetTesting = testing.map{f => MeshIO.readMesh(f).get}
    val spec : Seq[Seq[Double]] = for(gp <- limited) yield {
      // Average distance
      distancesEval(gp, datasetTesting, nb, distance)
    }

    // Write to file
    Utils.writeToFile(spec, outputPath)

  }

  // For each instance generated from the model returns the minimum distances to the testing set
  def distancesEval(gpModel: StatisticalMeshModel, data: Iterable[TriangleMesh[_3D]], nbSamples: Int, distance: String): Seq[Double] = {

    for (i <- 0 until nbSamples) yield {

      val sample = gpModel.sample()(scalismo.utils.Random.apply(0))

      val dist = distance.toLowerCase match {

        case "avg" =>
          data.map { m =>
            MeshMetrics.avgDistance(m, sample)
          }.min

        case "rms" =>
          data.map { m =>
            MeshMetrics.procrustesDistance(m, sample)
          }.min

        case "hausdorff" =>
          data.map { m =>
            Hausdorff.modifiedHausdorffDistance(m, sample)
          }.min
      }
      dist
    }
  }

}