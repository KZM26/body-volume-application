package build

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util

import breeze.linalg.DenseVector
import javafx.scene.shape.TriangleMesh
import scalismo.common.{Vectorizer, _}
import scalismo.io.MeshIO
import scalismo.ui.api.ScalismoUI
import scalismo.io._
import scalismo.geometry._
import scalismo.kernels
import scalismo.kernels._
import scalismo.mesh.TriangleMesh3D
import scalismo.registration._
import scalismo.statisticalmodel.{DiscreteLowRankGaussianProcess, GaussianProcess, LowRankGaussianProcess, StatisticalMeshModel}
import scalismo.statisticalmodel.dataset.{DataCollection, DataItem}
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.numerics.RandomMeshSampler3D
import scalismo.geometry.Point.Point3DVectorizer
import scalismo.common.Vectorizer
import scalismo.common.DiscreteDomain

import scala.collection.mutable.ListBuffer
import scalismo.utils.Random

class build{

  def start(): Unit = {

    val preprocessorConfig = List("Build shape model (b)", "Fun (f)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n")).toLowerCase()
      input match {

        case "b" => // Start landmarking
          this.build()

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

    println("Starting build process\n")

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
    val dataset = files.map{f => MeshIO.readMesh(f).get}

    println("Data loaded")

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
    // Gaussian Kernel
    val s : Double = 10.0
    val l : Double = 10.0

    // Use the mean shape as the reference shape
    val gaussKer : PDKernel[_3D] = GaussianKernel[_3D] (l) * s
    val matrixValuedGaussian : MatrixValuedPDKernel[_3D] = DiagonalKernel.apply(gaussKer, 3)
    matrixValuedGaussian(pcaMean.pointSet.point(PointId(0)), pcaMean.pointSet.point(PointId(1)))


    val discreteCov : DiscreteMatrixValuedPDKernel[_3D] = pcaModel.get.gp.cov
    val gpSSM = pcaModel.get.gp.interpolate(NearestNeighborInterpolator[_3D, Vector[_3D]]())
    val SSMKernel = gpSSM.cov

    val sampler = RandomMeshSampler3D(
      pcaMean,
      numberOfPoints = 300,
      seed = 42,
    )(Random.apply(0))

    val gp = GaussianProcess(zeroMean, SSMKernel)

    val lowRankGP = LowRankGaussianProcess.approximateGP(
      gp,
      sampler,
      numBasisFunctions = 1000,
    )(ThreeDSpace, vectorizer = gp.vectorizer, rand = Random.apply(0))

    val gpModel = StatisticalMeshModel(pcaMean, lowRankGP)

    val ui = ScalismoUI()
    ui.show(gpModel, "GP")
  }

}