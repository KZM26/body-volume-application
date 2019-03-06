package validation

import java.io._
import java.util._

import build.Build
import scalismo.geometry._3D
import scalismo.io._
import scalismo.statisticalmodel.dataset._
import scalismo.statisticalmodel._
import scalismo.mesh._

object Specificity {

  def specificity(datasetPath : String, distance : String, nb : Int, outputPath : String) {
    scalismo.initialize()

    // Build GPMM from DataCollection
    val dataset = new File(datasetPath).listFiles.map{f => MeshIO.readMesh(f).get}
    val path = "data/ref_landmarks/refLandmarks.json"
    val alignedSet = Build.alignMesh(dataset, path)

    val dc : DataCollection = DataCollection.fromMeshSequence(alignedSet.head, alignedSet.tail)(scalismo.utils.Random.apply(0))._1.get
    val gpModel : StatisticalMeshModel = Build.BuildGP(dc)

    // Varying GP rank
    val limited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModel, alignedSet.size)

    // Specificity

    val spec : Seq[Seq[Double]] = for(gp <- limited) yield {
      distancesEval(gp, alignedSet, nb, distance) //average distance
    }

    //write to file
    Utils.writeToFile(spec, outputPath)

  }

  // For each instance generated from the model returns the minimum distances to the testing set
  def distancesEval(gpModel: StatisticalMeshModel, data: Iterable[TriangleMesh[_3D]], nbSamples: Int, distance: String): Seq[Double] = {

    for (i<- 0 until nbSamples) yield {

      val sample = gpModel.sample()(scalismo.utils.Random.apply(0))

      val dist = distance.toLowerCase match {

        case avg =>
          data.map { m =>
            MeshMetrics.avgDistance(m, sample)
          }.min

        case rms =>
          data.map { m =>
            MeshMetrics.procrustesDistance(m, sample)
          }.min

        case hausdorff =>
          data.map { m =>
            Hausdorff.modifiedHausdorffDistance(m, sample)
          }.min
      }
      dist
    }
  }

}