package validation

import java.io.File

import build.Build
import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.statisticalmodel.dataset.DataCollection
import tools.Utils

object Generalisation {

  def generalisation(datasetPath : String, distance : String, outputPath : String): Unit = {
    scalismo.initialize()

    // Build GPMM from aligned data

    val dataset = new File(datasetPath).listFiles.map{f => MeshIO.readMesh(f).get}

    val path = "data/ref-landmarks/refLandmarks.json"
    val alignedSet = Build.alignMesh(dataset, path)

    val dc : DataCollection = DataCollection.fromMeshSequence(alignedSet.head, alignedSet.tail)(scalismo.utils.Random.apply(0))._1.get

    // Calculate Generalisation
    val folds : Seq[Seq[Double]] = createCrossValidation(dc, distance)

    // Write to file
    writeToFile(folds, outputPath)
  }

  // Withdraws one instance of the dataset, creates a training set with the others,
  // builds GPMM, measures the distance between the withdrawn instance and its projection onto the GPMM
  def createCrossValidation(dc: DataCollection, distance: String): Seq[Seq[Double]] = {

    val shuffledDataItems = scala.util.Random.shuffle(dc.dataItems)
    val foldSize = shuffledDataItems.size / dc.size
    val dataGroups = shuffledDataItems.grouped(foldSize).toSeq

    val distances = for (currFold <- 0 until dc.size) yield {
      // Testing collection with one instance
      val testingDataItems = dataGroups(currFold)
      val testingCollection = DataCollection(dc.reference, testingDataItems)(scalismo.utils.Random.apply(0))
      val testMesh : TriangleMesh[_3D] = dc.reference.transform(testingCollection.dataItems.head.transformation)

      // Training collection with all the other instances
      val trainingDataItems = dataGroups.slice(0, currFold).flatten ++: dataGroups.slice(currFold + 1, dataGroups.size).flatten
      val trainingCollection = DataCollection(dc.reference, trainingDataItems)(scalismo.utils.Random.apply(0))

      // Build GPMM using DC
      val gpModel = Build.BuildGP(trainingCollection)

      // Seq of reduced GPMM reduced to first n components
      val limited : Seq[StatisticalMeshModel] = Utils.gpVaryingRank(gpModel, trainingDataItems.size)

      // Distance between instance and its projection
      distancesEval(limited, testMesh, distance)
    }
    distances
  }

  def distancesEval(gpModelSeq: Seq[StatisticalMeshModel], projMesh: TriangleMesh[_3D], distance: String) : Seq[Double] = {

    val distances = for(pca<- gpModelSeq) yield {
      val proj : TriangleMesh[_3D] = pca.project(projMesh)
      MeshMetrics.avgDistance(proj,projMesh)
      val dist = distance.toLowerCase match {

        case "avg" =>
          MeshMetrics.avgDistance(proj,projMesh)

        case "rms" =>
          MeshMetrics.procrustesDistance(proj,projMesh)

        case "hausdorff" =>
          Hausdorff.modifiedHausdorffDistance(proj,projMesh)
      }
      dist
    }
    distances
  }

  def transpose(crossValidation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    for(i<- crossValidation.head.indices) yield {
      for(j<- crossValidation.head.indices) yield {
        crossValidation(j)(i)
      }
    }
  }

  def writeToFile(matrix: Seq[Seq[Double]], outputPath : String): Unit = {
    val writer = Utils.openFile(outputPath)
    for(j <- matrix.indices) yield {
      writer.write("instance n°"+j+"\r\n")

      for( i<- matrix(j).indices) {
        writer.write(matrix(j)(i).toString+" ")
      }

      writer.write("\r\n")
    }

    val tfolds : Seq[Seq[Double]] = transpose(matrix) //lines and columns to be transposed as in specificity
    Utils.writeMean(tfolds, writer)
    writer.close()
  }

}
