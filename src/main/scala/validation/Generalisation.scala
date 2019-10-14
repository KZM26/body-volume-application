package validation

import java.io.File

import build.Build
import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.registration.{LandmarkRegistration, RigidTransformation}

import scala.util.Random
import tools.Utils

object Generalisation {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def generalisation(datasetPath: String, distance: String, outputPath: String): Unit = {
    scalismo.initialize()

    val maleTraining = new File("data/spring-training/male-training/").listFiles
    val maleTesting = new File("data/spring-training/male-testing/").listFiles
    val maleCombined = Random.shuffle((maleTraining ++ maleTesting).sortBy{f => f.getName}.toList).take(100).map{f => MeshIO.readMesh(f).get}
    val maleDC = DataCollection.fromMeshSequence(maleCombined.head, maleCombined.tail)._1.get

    // Calculate Generalisation
    val maleFolds : Seq[Seq[Double]] = createCrossValidation(maleDC, distance)
    val outputPathMale = "data/generalisationMale.csv"
    writeToFile(maleFolds, outputPathMale)

    val femaleTraining = new File("data/spring-training/female-training/").listFiles
    val femaleTesting = new File("data/spring-training/female-testing/").listFiles
    val femaleCombined = Random.shuffle((femaleTraining ++ femaleTesting).sortBy{f => f.getName}.toList).take(100).map{f => MeshIO.readMesh(f).get}
    val femaleDC = DataCollection.fromMeshSequence(femaleCombined.head, femaleCombined.tail)._1.get

    // Calculate Generalisation
    val femaleFolds : Seq[Seq[Double]] = createCrossValidation(femaleDC, distance)
    val outputPathFemale = "data/generalisationFemale.csv"
    writeToFile(femaleFolds, outputPathFemale)

  }

  // Withdraws one instance of the dataset, creates a training set with the others,
  // builds GPMM, measures the distance between the withdrawn instance and its projection onto the GPMM
  def createCrossValidation(dc: DataCollection, distance: String): Seq[Seq[Double]] = {

    val shuffledDataItems = scala.util.Random.shuffle(dc.dataItems)
    val foldSize = shuffledDataItems.size/dc.size
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    // Take a sampke of the data items for cross validation. Done to reduce execution time
    val distances = for (currFold <- 0 until dc.size) yield {
      //println("Test: " + currFold.toString)
      // Testing collection with one instance
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollection(dc.reference, testingDataItems)
      val testMesh : TriangleMesh[_3D] = dc.reference.transform(testingCollection.dataItems.head.transformation)

      // Training collection with all the other instances
      val trainingDataItems = dataGroups.slice(0, currFold).flatten ++ dataGroups.slice(currFold + 1, dataGroups.size).flatten
      val trainingCollection = DataCollection(dc.reference, trainingDataItems)

      // Build GPMM using DC
      val gpModel = Build.buildGP(trainingCollection)

      // Seq of reduced GPMM reduced to first n components
      val limited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModel, trainingDataItems.size)

      //println("Completed Test: " + currFold.toString + "\nRemaining Test" (dc.size - currFold).toString)

      // Distance between instance and its projection
      distancesEval(limited, testMesh, distance)
    }
    distances
  }

  def distancesEval(gpModelSeq: Seq[StatisticalMeshModel], projMesh: TriangleMesh[_3D], distance: String) : Seq[Double] = {

    val distances = for(pca <- gpModelSeq) yield {

      val proj : TriangleMesh[_3D] = pca.project(projMesh)
      val dist = distance.toLowerCase match {

        case "avg" =>
          MeshMetrics.avgDistance(proj, projMesh)

        case "rms" =>
          MeshMetrics.procrustesDistance(proj, projMesh)

        case "hausdorff" =>
          Hausdorff.modifiedHausdorffDistance(proj, projMesh)
      }
      dist
    }
    distances
  }

  def transpose(crossValidation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    for(i <- crossValidation.head.indices) yield {
      for(j <- crossValidation.head.indices) yield {
        crossValidation(j)(i)
      }
    }
  }

  def writeToFile(matrix: Seq[Seq[Double]], outputPath : String): Unit = {
    val writer = Utils.openFile(outputPath)
    for(j <- matrix.indices) yield {
      writer.write("instance n°"+j+"\r\n")

      for(i <- matrix(j).indices) {
        writer.write(matrix(j)(i).toString+" ")
      }

      writer.write("\r\n")
    }

    val tfolds : Seq[Seq[Double]] = transpose(matrix) //lines and columns to be transposed as in specificity
    Utils.writeMean(tfolds, writer)
    writer.close()
  }

}
