package validation

import java.io._

import build.Build
import scalismo.geometry._3D
import scalismo.io._
import scalismo.statisticalmodel.dataset._
import scalismo.statisticalmodel._
import scalismo.mesh._
import scalismo.registration.{LandmarkRegistration, RigidTransformation}

import scala.util.Random
import tools.Utils

object Specificity {
  
  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def specificity(datasetPath: String, distance: String, nb: Int, outputPath: String) {
    scalismo.initialize()

    println("Starting Male Specificity")

    val maleTraining = Random.shuffle(new File("data/spring-training/male-training/").listFiles.sortBy{f => f.getName}.toList).take(100).map{f => MeshIO.readMesh(f).get}
    val maleTesting = new File("data/spring-training/male-testing/").listFiles.sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val maleDC = DataCollection.fromMeshSequence(maleTraining.head, maleTraining.tail)._1.get
    val gpModelMale : StatisticalMeshModel = Build.buildGP(maleDC)
    // Varying GP rank
    val maleLimited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModelMale, maleTraining.length)

    // Specificity
    val maleSpec : Seq[Seq[Double]] = for(gp <- maleLimited) yield {
      // Average distance
      distancesEval(gp, maleTesting, nb, distance)
    }

    val outputPathMale = "data/specificityMale.csv"
    Utils.writeToFile(maleSpec, outputPathMale)

    println("Starting Female Specificity")

    val femaleTraining = Random.shuffle(new File("data/spring-training/female-training/").listFiles.sortBy{f => f.getName}.toList).take(100).map{f => MeshIO.readMesh(f).get}
    val femaleTesting = new File("data/spring-training/female-testing/").listFiles.sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val femaleDC = DataCollection.fromMeshSequence(femaleTraining.head, femaleTraining.tail)._1.get
    val gpModelFemale : StatisticalMeshModel = Build.buildGP(femaleDC)
    // Varying GP rank
    val femaleLimited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModelFemale, femaleTesting.length)

    // Specificity
    val femaleSpec : Seq[Seq[Double]] = for(gp <- femaleLimited) yield {
      // Average distance
      distancesEval(gp, femaleTesting, nb, distance)
    }

    val outputPathFemale = "data/specificityFemale.csv"
    Utils.writeToFile(femaleSpec, outputPathFemale)

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
            val transform = LandmarkRegistration.rigid3DLandmarkRegistration(sample.pointSet.points.toIndexedSeq.zip(m.pointSet.points.toIndexedSeq), Utils.computeCentreOfMass(m))
            Hausdorff.modifiedHausdorffDistance(m, sample.transform(transform))
          }.min
      }

      dist
    }
  }

}