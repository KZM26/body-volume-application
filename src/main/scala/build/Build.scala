package build

import java.io.File
import java.io.PrintWriter

import scalismo.common.{Vectorizer, _}
import scalismo.io.MeshIO
import scalismo.io._
import scalismo.geometry._
import scalismo.kernels._
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}
import scalismo.statisticalmodel.dataset.DataCollection
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.numerics.RandomMeshSampler3D

import scala.collection.parallel._

object Build {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    val builder = new ModelBuild
    builder.build()
  }

  def buildGP (dc: DataCollection): StatisticalMeshModel = {
    scalismo.initialize()
    val builder = new ModelBuild
    val model = builder.BuildGP(dc)
    model
  }
/*
  def start(): Unit = {

    val preprocessorConfig = List("Build shape model (b)", "Experiments (e)", "Help (h)", "Return (r)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n")).toLowerCase()
      input match {

        case "b" => // Start landmarking
          build()

        case "e" =>
          experiments()

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "r" => // Return
          return

        case _ => // Any
          println("That ain't it chief\n")
      }
    }
  }*/
}

object Validate {
  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def main(args: Array[String]): Unit = {
    scalismo.initialize()
    val builder = new ModelBuild
    builder.experiments()
  }
}

case class ModelBuild() {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def build(): Unit = {

    //println("Starting build process")
    // First load files

    val maleTraining = new File("data/spring-training/male-training/").listFiles
    val maleTesting = new File("data/spring-training/male-testing/").listFiles
    val maleCombined = (maleTraining ++ maleTesting).sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val maleDC = DataCollection.fromMeshSequence(maleCombined.head, maleCombined.tail)._1.get

    val femaleTraining = new File("data/spring-training/female-training/").listFiles
    val femaleTesting = new File("data/spring-training/female-testing/").listFiles
    val femaleCombined = (femaleTraining ++ femaleTesting).sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val femaleDC = DataCollection.fromMeshSequence(femaleCombined.head, femaleCombined.tail)._1.get

    //println("Data collection created")

   val gpMaleModel = BuildGP(maleDC)

   val gpFemaleModel = BuildGP(femaleDC)

    //println("Displaying Model")

    //val ui = ScalismoUI()
    //ui.show(gpModel, "fbm")
    StatismoIO.writeStatismoMeshModel(gpMaleModel, new File("data/maleFBM.h5"))
    StatismoIO.writeStatismoMeshModel(gpFemaleModel, new File("data/femaleFBM.h5"))
    /*var input = ""

    while (input != "y" || input != "n") {
      input = scala.io.StdIn.readLine("Save model? (y/n)\n").toLowerCase()
      input match {

        case "y" => // Save model
          println("Saving model")
          StatismoIO.writeStatismoMeshModel(gpModel, new File("data/fbm.h5"))
          println("Model saved")
          return

        case "n" => // Don't save model
          return

        case _ => // Any
          println("That ain't it chief\n")
      }
    }
*/

  }

  def experiments(): Unit = {
    // Metrics: Specificity, Generalisation, and compactness

    val gpModelMale = StatismoIO.readStatismoMeshModel(new File("data/maleFBM.h5")).get
    val gpModelFemale = StatismoIO.readStatismoMeshModel(new File("data/femaleFBM.h5")).get

    val datasetPath = "data/spring-training/"
    val distance = "hausdorff"
    val nb = 100
    val outputSpec = "data/specificity.csv"
    val outputGen = "data/generalisation.csv"

    // Leave out 1 construction to test Generalisation
    // Construct shape models leaving out 1 at each construction and measure Generalisation
    println("Calculating Generalisation")
    validation.Generalisation.generalisation(datasetPath, distance, outputGen)

    // Specificity
    // 10000 samples as per Pishchulin 2017 and Styner 2003
    //println("Calculating Specificity")
    //validation.Specificity.specificity(datasetPath, distance, nb, outputSpec)

    // Compactness
    /*
    println("Determining Compactness")
    val compactnessMale = gpModelMale.rank
    val eigenMale = gpModelMale.gp.klBasis.map{k =>
      k.eigenvalue
    }
    val compactnessFemale = gpModelFemale.rank
    val eigenFemale = gpModelFemale.gp.klBasis.map{k =>
      k.eigenvalue
    }

    val writerMale = new PrintWriter(new File("data/compactnessMale.txt"))
    writerMale.write(compactnessMale.toString)
    writerMale.close()
    val writerEigenMale = new PrintWriter(new File("data/compactnessEigenMale.txt"))
    writerEigenMale.write(eigenMale.mkString("\n"))
    writerEigenMale.close()

    val writerFemale = new PrintWriter(new File("data/compactnessFemale.txt"))
    writerFemale.write(compactnessFemale.toString)
    writerFemale.close()
    val writerEigenFemale = new PrintWriter(new File("data/compactnessEigenFemale.txt"))
    writerEigenFemale.write(eigenFemale.mkString("\n"))
    writerEigenFemale.close()

    println("Writing complete")*/
  }


  /**
    * Builds GPMM
    *
    * Parameters:
    *    -   `dc` Data collection of meshes
    * Returns:
    *    -  Statistical mesh model
    */
  def BuildGP(inputDC: DataCollection): StatisticalMeshModel = {

    val dc = DataCollection.gpa(inputDC)
    val pcaModel = StatisticalMeshModel.createUsingPCA(dc)
    val referenceMesh = dc.reference
    // The zero mean
    val zeroMean = VectorField(RealSpace[_3D], (pt:Point[_3D]) => EuclideanVector(0,0,0))

    // val discreteCov : DiscreteMatrixValuedPDKernel[_3D] = pcaModel.get.gp.cov
    // val gpSSM = pcaModel.get.gp.interpolate(NearestNeighborInterpolator[_3D, Vector[_3D]]())
    val gpSSM = pcaModel.get.gp.interpolateNystrom(500)
    val SSMKernel = gpSSM.cov

    val sampler = RandomMeshSampler3D(
      referenceMesh,
      numberOfPoints = 800,
      seed = 0
    )

    val theKernel = SSMKernel
    val gp = GaussianProcess(zeroMean, theKernel)

    val lowRankGP = LowRankGaussianProcess.approximateGPNystrom(
      gp,
      sampler,
      numBasisFunctions = 200
    )(ThreeDSpace, vectorizer = gp.vectorizer)


    val gpModel = StatisticalMeshModel(referenceMesh, lowRankGP)

    gpModel
  }
}
