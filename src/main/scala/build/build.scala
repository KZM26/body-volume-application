package build

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util

import scalismo.common.{Vectorizer, _}
import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI
import scalismo.io._
import scalismo.geometry._
import scalismo.kernels._
import scalismo.registration._
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel, dataset}
import scalismo.statisticalmodel.dataset.{DataCollection, DataItem}
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.numerics.RandomMeshSampler3D
import scala.collection.mutable.ListBuffer
import scalismo.utils.Random
import scala.util.Try

class build{

  def start(): Unit = {

    val preprocessorConfig = List("Build shape model (b)", "Experiments (e)", "Fun (f)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n")).toLowerCase()
      input match {

        case "b" => // Start landmarking
          this.build()

        case "e" =>
          this.experiments()

        case "f" => // Start Alignment
          // TODO

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

  def build(): Unit = {

    println("Starting build process")

    // Check if refLandmarks.json exists - Landmark file
    if (!Files.exists(Paths.get("data/ref_landmarks/refLandmarks.json"))) {
      println("Landmark file - refLandmarks.json not found. Returning to previous menu")
      return
    }

    // If exists, start alignment
    // Initialise Scalismo
    scalismo.initialize()

    println("Scalismo initialised")

    // First load files
    val files = new File("data/training/").listFiles


    println("Data loaded")

   val gpModel = this.BuildGP(files)

    println("Displaying Model")

    val ui = ScalismoUI()
    ui.show(gpModel, "fbm")

    println("Saving GPMM")

    StatismoIO.writeStatismoMeshModel(gpModel, new File("data/fbm.h5"))

    println("GPMM Saved")

  }

  def experiments(): Unit = {
    // Use model metrics
    // See https://github.com/unibas-gravis/scalismo/blob/071148d7a5193efa1bc60282f36c6e160a258efc/src/main/scala/scalismo/statisticalmodel/dataset/ModelMetrics.scala
    // Metrics: Specificity, Generalisation, and compactness

    scalismo.initialize()

    println("Scalismo initialised")

    val gpModel = StatismoIO.readStatismoMeshModel(new File("data/fbm.h5")).get

    val files = new File("data/training/").listFiles
    val dataset = files.map{f => MeshIO.readMesh(f).get}
    var generalisation = new ListBuffer[Try[Double]]()
    // 10000 samples as per Pishchulin 2017 and Styner 2003
    val specificity = scalismo.statisticalmodel.dataset.ModelMetrics.specificity(gpModel, dataset, 10000)(Random.apply(0))

    // Leave out 1 construction to test Generalisation
    // Construct shape models leaving out 1 at each construction and measure Generalisation
    for (i <- 0 until files.length - 1){

      var fileCopy = files
      var fileTrunc : Array[File] = null

      if (i == 0){
        fileTrunc = fileCopy.drop(1)
      }
      else if (i == files.length - 1){
        fileTrunc = fileCopy.take(i)
      }
      else{
        fileTrunc = fileCopy.take(i) ++ fileCopy.drop(i + 1)
      }

      val shapeModelReduced = this.BuildGP(fileTrunc)
      val dataSet = fileTrunc.map{f => MeshIO.readMesh(f).get}

      val dc = DataCollection.fromMeshSequence(dataSet.head, dataSet.toIndexedSeq)(Random.apply(0))._1.get
      generalisation += scalismo.statisticalmodel.dataset.ModelMetrics.generalization(shapeModelReduced, dc)
    }
  }

  def BuildGP(fileSet : Array[File]) : StatisticalMeshModel = {

    val dataset = fileSet.map{f => MeshIO.readMesh(f).get}

    println("Building GPMM")
    // The reference landmarks are based on shape 0 in the training set
    val refLandmarks = LandmarkIO.readLandmarksJson[_3D](new File("data/ref_landmarks/refLandmarks.json")).get
    val reference = dataset.head

    // Extract the point IDs from the reference shape using the landmark coordinates
    // Reference landmark iterator
    // List buffer for points
    val it = refLandmarks.seq.iterator
    var pointIDs = new ListBuffer[Int]()

    // Iterate, get point, extract pid from the reference shape
    while (it.hasNext){
      val pt = it.next().point
      val pid = reference.pointSet.pointId(pt)
      pointIDs += pid.get.id
    }

    // Get the shapes to align to the reference
    val toAlign = dataset.tail

    println("Starting alignment")

    val refLandmarksProper = pointIDs.map{id => Landmark("L_"+id, reference.pointSet.point(PointId(id))) }
    // Find a rigid transform for each in the to align set and apply it
    val alignedSet = dataset.map{ mesh =>
      val landmarks = pointIDs.map{id => Landmark[_3D]("L_"+id, mesh.pointSet.point(PointId(id)))}
      // TODO: Figure out what the centre should actually be
      val rigidTrans = LandmarkRegistration.rigid3DLandmarkRegistration(landmarks, refLandmarksProper, Point3D(0, 0, -10))
      mesh.transform(rigidTrans)
    }

    println("Alignment complete. Writing to file (JK)")

    // Create deformation fields from aligned data
    // Build & save PCA model
    // Extract covariance matrix from it
    // Create GP kernels and combine with the SSM matrix
    // Build GPSSM following standard steps
    println("Calculating Deformation Fields")

    val defFields = alignedSet.map{ m =>
      val deformationVectors = pointIDs.map{ id : Int =>
        m.pointSet.point(PointId(id)) - reference.pointSet.point(PointId(id))
      }.toIndexedSeq

      DiscreteField[_3D, UnstructuredPointsDomain[_3D], Vector[_3D]](reference.pointSet, deformationVectors)
    }.toIndexedSeq

    val dc = DataCollection.fromMeshSequence(reference, alignedSet.toIndexedSeq)(Random.apply(0))._1.get
    val pcaModel = StatisticalMeshModel.createUsingPCA(dc)
    val pcaMean = pcaModel.get.mean
    // The zero mean
    val zeroMean = VectorField(RealSpace[_3D], (pt:Point[_3D]) => Vector(0,0,0))

    // Kernel Set
    // TODO: Combine kernels and see what happens
    // Gaussian Kernel
    val s : Double = 0.10
    val l : Double = 10.0

    println("Calculating Kernels")

    // Use the mean shape as the reference shape
    val gaussKer : PDKernel[_3D] = GaussianKernel[_3D] (l) * s
    val matrixValuedGaussian : MatrixValuedPDKernel[_3D] = DiagonalKernel.apply(gaussKer, 3)
    val linearPDKernel = LinearKernel() * 0.01

    // val discreteCov : DiscreteMatrixValuedPDKernel[_3D] = pcaModel.get.gp.cov
    // val gpSSM = pcaModel.get.gp.interpolate(NearestNeighborInterpolator[_3D, Vector[_3D]]())
    val gpSSM = pcaModel.get.gp.interpolateNystrom(500)(Random.apply(0))
    val SSMKernel = gpSSM.cov

    val simLinear = SymmetriseKernel(linearPDKernel)
    val augmentedKernel = (SSMKernel + simLinear) * matrixValuedGaussian

    val sampler = RandomMeshSampler3D(
      pcaMean,
      numberOfPoints = 500,
      seed = 0,
    )(Random.apply(0))

    val theKernel = SSMKernel
    val gp = GaussianProcess(zeroMean, theKernel)

    val lowRankGP = LowRankGaussianProcess.approximateGP(
      gp,
      sampler,
      numBasisFunctions = 150,
    )(ThreeDSpace, vectorizer = gp.vectorizer, rand = Random.apply(0))


    val gpModel = StatisticalMeshModel(pcaMean, lowRankGP)

    gpModel
  }

  def SymmetriseKernel(ker : PDKernel[_3D]) : MatrixValuedPDKernel[_3D] = {
    val xmirrored = XmirroredKernel(ker)
    val k1 = DiagonalKernel(ker, 3)
    val k2 = DiagonalKernel(xmirrored * -1f, xmirrored, xmirrored)
    k1 + k2
  }

}



case class LinearKernel() extends PDKernel[_3D] {
  override def domain = RealSpace[_3D]
  override def k(x: Point[_3D], y: Point[_3D]) = {
    x.toVector dot y.toVector
  }
}

case class XmirroredKernel(ker : PDKernel[_3D]) extends PDKernel[_3D] {
  override def domain = RealSpace[_3D]
  override def k(x: Point[_3D], y: Point[_3D]) = ker(Point(x(0) * -1f ,x(1), x(2)), y)
}