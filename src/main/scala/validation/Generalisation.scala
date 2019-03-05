package validation

import build.Build
import java.io.File

import scalismo.geometry._3D
import scalismo.io.MeshIO
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.statisticalmodel.StatisticalMeshModel
import scalismo.statisticalmodel.dataset.{DataCollection, PCAModel}

object Generalisation {

  def generalisation(datasetPath : String, distance : String, outputPath : String) = {
    scalismo.initialize()

    // Build GPMM from aligned data
    val dataset = new File(datasetPath).listFiles.map{f => MeshIO.readMesh(f).get}

    val path = "data/ref_landmarks/refLandmarks.json"
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

      val pcaModel : StatisticalMeshModel = PCAModel.buildModelFromDataCollection(trainingCollection).get
      //table of reduced PCA to first n components
      val limited : Seq[StatisticalMeshModel] = Specificity.varyingPCA(pcaModel, trainingDataItems.size)
      //distance between instance and its projection
      distancesEval(limited, testMesh, distance)
    }
    distances
  }

  def distancesEval(pcaModelSeq: Seq[StatisticalMeshModel], projMesh: TriangleMesh, distance: String) : Seq[Double] = {
    val distances = for(pca<- pcaModelSeq) yield {
      val proj : TriangleMesh = pca.project(projMesh)
      MeshMetrics.avgDistance(proj,projMesh)
      val dist = distance match {
        case avg => MeshMetrics.avgDistance(proj,projMesh)
        case rms => MeshMetrics.procrustesDistance(proj,projMesh)
        case hausdorff => Hausdorff.modifiedHausdorffDistance(proj,projMesh)
      }
      dist
    }
    distances
  }

  def transpose(crossValidation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    for(i<- 0 until crossValidation(0).size) yield {
      for(j<- 0 until crossValidation.size) yield {
        crossValidation(j)(i)
      }
    }
  }

  def writeToFile(matrix: Seq[Seq[Double]], outputPath : String) = {
    val writer = Specificity.openFile(outputPath)
    for(j<- 0 until matrix.size) yield {
      writer.write("instance n°"+j+"\r\n")
      for(i<- 0 until matrix(j).size) {
        writer.write(matrix(j)(i).toString+" ")
      }
      writer.write("\r\n")
    }

    val tfolds : Seq[Seq[Double]] = transpose(matrix) //lines and columns to be transposed as in specificity
    Specificity.writeMean(tfolds, writer)
    writer.close()
  }

}
