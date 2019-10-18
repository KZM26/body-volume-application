package fitting

import java.io.{File, PrintWriter}

import javax.imageio.ImageIO

import scala.io.Source
import breeze.linalg.{DenseMatrix, DenseVector}
import measurement.Measurement
import scalismo.common._
import scalismo.faces.io.TLMSLandmarksIO
import scalismo.geometry._
import scalismo.io.{LandmarkIO, MeshIO, StatismoIO}
import scalismo.mesh.{MeshMetrics, TriangleMesh}
import scalismo.registration.LandmarkRegistration
import scalismo.statisticalmodel.{GaussianProcess, LowRankGaussianProcess, MultivariateNormalDistribution, StatisticalMeshModel}
import scalismo.ui.api.{Group, ScalismoUI}
import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.geometry.Dim.ThreeDSpace
import scalismo.kernels.{DiagonalKernel, GaussianKernel, MatrixValuedPDKernel, PDKernel}
import scalismo.numerics.RandomMeshSampler3D
import tools.Utils
import tools.Utils.sexEnum
import validation.Hausdorff

import scala.collection.mutable.ListBuffer

object Fitting {

  implicit private val rng: scalismo.utils.Random = scalismo.utils.Random(42)

  def start(): Unit = {
    val fittingConfig = List("Start image fitting (f)", "Experiments (e)", "Help (h)", "Return (r)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(fittingConfig.mkString("\n")).toLowerCase()
      input match {

        case "f" => // Start fitting
          //fittingExperimentFemale()
          fittingExperimentMale()

        case "e" => // Start experiments
          fittingTestLandmark()

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "r" => // Return
          return

        case _ => // Any
          println("That ain't it chief\n")
      }
    }
  }

  def fittingExperimentMale(){

    val rootDirectory = "data/male-test/"
    val meshLm = LandmarkIO.readLandmarksJson[_3D](new File("data/mpi-training/mpi.json")).get.sortBy{lm => lm.id}
    val meshReference = MeshIO.readMesh(new File("data/mpi-training/reference.stl")).get
    val meshWaistAntID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.anterior").head.point).id
    val meshWaistPostID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.posterior").head.point).id

    val frontNoise = 0.5
    val sideNoise = 0.001

    val maleModel = StatismoIO.readStatismoMeshModel(new File("data/maleFBM.h5")).get

    val maleModelLmA = new File("data/fbm-landmarks/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(!_.getName.contains("female")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val maleModelLmB = new File("data/fbm-landmarks/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(!_.getName.contains("female")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val maleModelLm = Utils.landmarkAverage(IndexedSeq(maleModelLmA, maleModelLmB)).head

    val baseMaleModelContainer = modelContainer(maleModel, maleModelLm)

    val augMaleModel = augmentFittingModel(maleModel, 1e-8, 200.0)
    val maleModelContainer = modelContainer(augMaleModel, maleModelLm)

    val marginalAugMaleModel = marginalModelArmCut(augMaleModel)
    val marginalMaleModelContainer = modelContainer(marginalAugMaleModel, maleModelLm)

    val maleMeshes = new File("data/mpi-male/").listFiles.filter{f => f.getName.contains("stl")}.sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val realHeight: IndexedSeq[Double] = maleMeshes.map{mesh => Measurement.measurePointHeight(mesh)}

    val minLandmarkFile = Source.fromFile("data/fitting-landmarks/min.txt")
    val minLandmark: IndexedSeq[String] = minLandmarkFile.getLines().toIndexedSeq
    minLandmarkFile.close()

    val medLandmarkFile = Source.fromFile("data/fitting-landmarks/med.txt")
    val medLandmark: IndexedSeq[String] = medLandmarkFile.getLines().toIndexedSeq
    medLandmarkFile.close()

    val imgLmFrontA = new File("data/image-male/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Front")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmFrontB = new File("data/image-male/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Front")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmFront = Utils.imageLandmarkAverage(IndexedSeq(imgLmFrontA, imgLmFrontB))

    val imageFiles = new File("data/image-male/").listFiles.filter{f => f.getName.contains("png")}.sortBy{f => f.getName}

    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i) * 1e3
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val imgLmSideA = new File("data/image-male/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Side")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmSideB = new File("data/image-male/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Side")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmSide = Utils.imageLandmarkAverage(IndexedSeq(imgLmSideA, imgLmSideB))

    val sideImages = imageFiles.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}

    val targetLmSide: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmSide.indices.map{i =>
      val lmHeightSide = imgLmSide(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmSide(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = sideImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightSide)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmSide(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val baseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, baseMaleModelContainer)
    }

    val targets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, maleModelContainer)
    }

    val marginalTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, marginalMaleModelContainer)
    }

    val inverseBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, targetLmFront(i), sideNoise, frontNoise)
      fittingModelContainer(fitData, baseMaleModelContainer)
    }

    val minBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i).filter{l =>
        val filtered = minLandmark.filter(_ == l.id)
        filtered.nonEmpty
      },
        targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}.filter{l =>
          val filtered = minLandmark.filter(_ == l.id)
          filtered.nonEmpty
        },
        frontNoise, sideNoise)
      fittingModelContainer(fitData, baseMaleModelContainer)
    }

    val medBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i).filter{l =>
        val filtered = medLandmark.filter(_ == l.id)
        filtered.nonEmpty
      },
        targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}.filter{l =>
          val filtered = medLandmark.filter(_ == l.id)
          filtered.nonEmpty
        },
        frontNoise, sideNoise)
      fittingModelContainer(fitData, baseMaleModelContainer)
    }

    val baseBestFits: IndexedSeq[TriangleMesh[_3D]] = baseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val bestFits: IndexedSeq[TriangleMesh[_3D]] = targets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val marginalBestFits: IndexedSeq[TriangleMesh[_3D]] = marginalTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val inverseBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = inverseBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val minBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = minBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val medBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = medBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val waistAntID = maleModel.referenceMesh.pointSet.findClosestPoint(maleModelLm.filter(p => p.id == "waist.anterior").head.point).id
    val waistPostID = maleModel.referenceMesh.pointSet.findClosestPoint(maleModelLm.filter(p => p.id == "waist.posterior").head.point).id

    val baseTestResults = baseBestFits.indices.map {i =>
      val bestFit = baseBestFits(i)
      val original = maleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(baseTestResults, rootDirectory + "BaseMale")

    val modBaseTestResults = bestFits.indices.map {i =>
        val bestFit = bestFits(i)
        val original = maleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

        val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

        (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(modBaseTestResults, rootDirectory + "ModBaseMale")

    val cutTestResults = bestFits.indices.map {i =>
        val bestFit = bestFits(i)
        val original = maleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val cutResult = modelMeshArmClip(bestFit)

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))
        val alignedCutOriginal = mpiFingerClip(alignedOriginal)

        val avgDistance = MeshMetrics.avgDistance(cutResult, alignedCutOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(cutResult, alignedCutOriginal)

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

        (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(cutTestResults, rootDirectory + "CutMale")

    val marginalTestResults = marginalBestFits.indices.map {i =>
        val bestFit = marginalBestFits(i)
        val original = maleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(marginalAugMaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))
        val alignedCutOriginal = mpiFingerClip(alignedOriginal)

        val avgDistance = MeshMetrics.avgDistance(bestFit, alignedCutOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedCutOriginal)

        val margWaistAntID = marginalAugMaleModel.referenceMesh.pointSet.findClosestPoint(maleModelLm.filter(p => p.id == "waist.anterior").head.point).id
        val margWaistPostID = marginalAugMaleModel.referenceMesh.pointSet.findClosestPoint(maleModelLm.filter(p => p.id == "waist.posterior").head.point).id

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(margWaistAntID), bestFit.pointSet.point(margWaistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

        (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(marginalTestResults, rootDirectory + "MarginalMale")

    val inverseBaseTestResults = inverseBaseBestFits.indices.map {i =>
      val bestFit = inverseBaseBestFits(i)
      val original = maleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(inverseBaseTestResults, rootDirectory + "InverseBaseMale")

    val minBaseTestResults = minBaseBestFits.indices.map {i =>
      val bestFit = minBaseBestFits(i)
      val original = maleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(minBaseTestResults, rootDirectory + "MinBaseMale")

    val medBaseTestResults = medBaseBestFits.indices.map {i =>
      val bestFit = medBaseBestFits(i)
      val original = maleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = maleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(maleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(medBaseTestResults, rootDirectory + "MedBaseMale")

    println("Tests complete. Saved to file")
  }

  def fittingExperimentFemale(){

    val rootDirectory = "data/female-test/"
    val meshLm = LandmarkIO.readLandmarksJson[_3D](new File("data/mpi-training/mpi.json")).get.sortBy{lm => lm.id}
    val meshReference = MeshIO.readMesh(new File("data/mpi-training/reference.stl")).get
    val meshWaistAntID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.anterior").head.point).id
    val meshWaistPostID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.posterior").head.point).id

    val frontNoise = 0.5
    val sideNoise = 0.05

    val femaleModel = StatismoIO.readStatismoMeshModel(new File("data/femaleFBM.h5")).get

    val femaleModelLmA = new File("data/fbm-landmarks/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(_.getName.contains("female")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val femaleModelLmB = new File("data/fbm-landmarks/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(_.getName.contains("female")).map{f => LandmarkIO.readLandmarksJson[_3D](f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val femaleModelLm = Utils.landmarkAverage(IndexedSeq(femaleModelLmA, femaleModelLmB)).head

    val baseFemaleModelContainer = modelContainer(femaleModel, femaleModelLm)

    val augFemaleModel = augmentFittingModel(femaleModel, 0.005, 20.0)
    val femaleModelContainer = modelContainer(augFemaleModel, femaleModelLm)

    val marginalAugFemaleModel = marginalModelArmCut(augFemaleModel)
    val marginalFemaleModelContainer = modelContainer(marginalAugFemaleModel, femaleModelLm)

    val femaleMeshes = new File("data/mpi-female/").listFiles.filter{f => f.getName.contains("stl")}.sortBy{f => f.getName}.map{f => MeshIO.readMesh(f).get}
    val realHeight: IndexedSeq[Double] = femaleMeshes.map{mesh => Measurement.measurePointHeight(mesh)}

    val minLandmarkFile = Source.fromFile("data/fitting-landmarks/min.txt")
    val minLandmark: IndexedSeq[String] = minLandmarkFile.getLines().toIndexedSeq
    minLandmarkFile.close()

    val medLandmarkFile = Source.fromFile("data/fitting-landmarks/med.txt")
    val medLandmark: IndexedSeq[String] = medLandmarkFile.getLines().toIndexedSeq
    medLandmarkFile.close()

    val imgLmFrontA = new File("data/image-female/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Front")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmFrontB = new File("data/image-female/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Front")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmFront = Utils.imageLandmarkAverage(IndexedSeq(imgLmFrontA, imgLmFrontB))

    val imageFiles = new File("data/image-female/").listFiles.filter{f => f.getName.contains("png")}.sortBy{f => f.getName}

    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i) * 1e3
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val imgLmSideA = new File("data/image-female/0/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Side")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmSideB = new File("data/image-female/1/").listFiles.filter(_.isDirectory).map{dir =>
      dir.listFiles().filter(f => f.getName.contains("tlms") && f.getName.contains("Side")).sortBy(_.getName).
        map{f => TLMSLandmarksIO.read2D(f).get.toIndexedSeq}.map{lms => lms.sortBy(_.id)}.toIndexedSeq
    }.toIndexedSeq

    val imgLmSide = Utils.imageLandmarkAverage(IndexedSeq(imgLmSideA, imgLmSideB))

    val sideImages = imageFiles.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmSide: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmSide.indices.map{i =>
      val lmHeightSide = imgLmSide(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmSide(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = sideImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightSide)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmSide(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val baseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, baseFemaleModelContainer)
    }

    val targets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, femaleModelContainer)
    }

    val marginalTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, frontNoise, sideNoise)
      fittingModelContainer(fitData, marginalFemaleModelContainer)
    }

    val inverseBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, targetLmFront(i), sideNoise, frontNoise)
      fittingModelContainer(fitData, baseFemaleModelContainer)
    }

    val minBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i).filter{l =>
        val filtered = minLandmark.filter(_ == l.id)
        filtered.nonEmpty
      },
        targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}.filter{l =>
          val filtered = minLandmark.filter(_ == l.id)
          filtered.nonEmpty
        },
        frontNoise, sideNoise)
      fittingModelContainer(fitData, baseFemaleModelContainer)
    }

    val medBaseTargets = targetLmFront.indices.map{i =>
      val fitData = fittingData(targetLmFront(i).filter{l =>
        val filtered = medLandmark.filter(_ == l.id)
        filtered.nonEmpty
      },
        targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}.filter{l =>
          val filtered = medLandmark.filter(_ == l.id)
          filtered.nonEmpty
        },
        frontNoise, sideNoise)
      fittingModelContainer(fitData, baseFemaleModelContainer)
    }

    val baseBestFits: IndexedSeq[TriangleMesh[_3D]] = baseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val bestFits: IndexedSeq[TriangleMesh[_3D]] = targets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val marginalBestFits: IndexedSeq[TriangleMesh[_3D]] = marginalTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val inverseBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = inverseBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val minBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = minBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val medBaseBestFits: IndexedSeq[TriangleMesh[_3D]] = medBaseTargets.map{target =>
      val posterior = landmarkPosterior(target)
      posterior.mean
    }

    val waistAntID = femaleModel.referenceMesh.pointSet.findClosestPoint(femaleModelLm.filter(p => p.id == "waist.anterior").head.point).id
    val waistPostID = femaleModel.referenceMesh.pointSet.findClosestPoint(femaleModelLm.filter(p => p.id == "waist.posterior").head.point).id

    val baseTestResults = baseBestFits.indices.map { i =>
      val bestFit = baseBestFits(i)
      val original = femaleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(baseTestResults, rootDirectory + "BaseFemale")

    val modBaseTestResults = bestFits.indices.map { i =>
        val bestFit = bestFits(i)
        val original = femaleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

        val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

        (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(modBaseTestResults, rootDirectory + "ModBaseFemale")

    val cutTestResults = bestFits.indices.map { i =>
        val bestFit = bestFits(i)
        val original = femaleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val cutResult = modelMeshArmClip(bestFit)

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))
        val alignedCutOriginal = mpiFingerClip(alignedOriginal)

        val avgDistance = MeshMetrics.avgDistance(cutResult, alignedCutOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(cutResult, alignedCutOriginal)

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(cutTestResults, rootDirectory + "CutFemale")

    val marginalTestResults = marginalBestFits.indices.map { i =>
        val bestFit = marginalBestFits(i)
        val original = femaleMeshes(i)

        val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
        val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(marginalAugFemaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

        val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))
        val alignedCutOriginal = mpiFingerClip(alignedOriginal)

        val avgDistance = MeshMetrics.avgDistance(bestFit, alignedCutOriginal)
        val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedCutOriginal)

        val margWaistAntID = marginalAugFemaleModel.referenceMesh.pointSet.findClosestPoint(femaleModelLm.filter(p => p.id == "waist.anterior").head.point).id
        val margWaistPostID = marginalAugFemaleModel.referenceMesh.pointSet.findClosestPoint(femaleModelLm.filter(p => p.id == "waist.posterior").head.point).id

        val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
        val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(margWaistAntID), bestFit.pointSet.point(margWaistPostID)) * 1e+3
        val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

        val realHeight = Measurement.measurePointHeight(original) * 1e+3
        val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
        val realVol = Measurement.getMeshVolume(original) * 1e3

        (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
      }
    writeExperimentToFile(marginalTestResults, rootDirectory + "MarginalFemale")

    val inverseBaseTestResults = inverseBaseBestFits.indices.map { i =>
      val bestFit = inverseBaseBestFits(i)
      val original = femaleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(inverseBaseTestResults, rootDirectory + "InverseBaseFemale")

    val minBaseTestResults = minBaseBestFits.indices.map {i =>
      val bestFit = minBaseBestFits(i)
      val original = femaleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(minBaseTestResults, rootDirectory + "MinBaseFemale")

    val medBaseTestResults = medBaseBestFits.indices.map {i =>
      val bestFit = medBaseBestFits(i)
      val original = femaleMeshes(i)

      val meshLms = meshLm.map { lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id)) }
      val resultLm = femaleModelLm.map { lm => new Landmark[_3D](lm.id, bestFit.pointSet.point(femaleModel.referenceMesh.pointSet.findClosestPoint(lm.point).id)) }

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(bestFit)))

      val avgDistance = MeshMetrics.avgDistance(bestFit, alignedOriginal)
      val hausdorff = Hausdorff.modifiedHausdorffDistance(bestFit, alignedOriginal)

      val heightPosterior = Measurement.measurePointHeight(bestFit) * 1e+3
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val realHeight = Measurement.measurePointHeight(original) * 1e+3
      val realWC = Measurement.getWaistCircumference(original, original.pointSet.point(meshWaistAntID), original.pointSet.point(meshWaistPostID)) * 1e3
      val realVol = Measurement.getMeshVolume(original) * 1e3

      (realHeight, heightPosterior, realWC, wcPosterior, realVol, volPosterior, avgDistance, hausdorff)
    }
    writeExperimentToFile(medBaseTestResults, rootDirectory + "MedBaseFemale")

    println("Tests complete. Saved to file")
  }

  def writeExperimentToFile(testResults: IndexedSeq[(Double, Double, Double, Double, Double, Double, Double, Double)], directoryTestType: String): Unit = {

    var allData = new ListBuffer[Seq[Any]]
    val csvFields = Seq("Mesh Height", "Best Fit Height", "Mesh WC", "Best Fit WC", "Mesh Volume", "Best Fit Vol", "Average Distance", "Hausdorff Distance")
    val directory = directoryTestType + "ImageTest.csv"

    val rh = testResults.map { result => result._1 }
    val fh = testResults.map { result => result._2 }
    val rwc = testResults.map { result => result._3 }
    val fwc = testResults.map { result => result._4 }
    allData += rh
    allData += fh
    allData += rwc
    allData += fwc
    allData += testResults.map { result => result._5 }
    allData += testResults.map { result => result._6 }
    allData += testResults.map { result => result._7 * 1e3 }  // Scale up surface distances
    allData += testResults.map { result => result.  _8 * 1e3 } // Scale up surface distances

    Utils.csvWrite(csvFields, allData.toList, directory)

    val (hmd, hmad) = Utils.getDifferences(rh, fh)
    val (wcmd, wcmad) = Utils.getDifferences(rwc, fwc)

    val writer = new PrintWriter(new File(directoryTestType + "ImageTestDifferences.txt"))
    writer.write("Height MD (real - fit): " + hmd.toString + "\n")
    writer.write("Height MAD: " + hmad.toString + "\n")
    writer.write("WC MD (real - fit): " + wcmd.toString + "\n")
    writer.write("WC MAD: " + wcmad.toString)
    writer.close()
  }

  def fittingTestLandmark(): Unit = {

    val maleModel = StatismoIO.readStatismoMeshModel(new File("data/maleFBM.h5")).get
    val femaleModel = StatismoIO.readStatismoMeshModel(new File("data/femaleFBM.h5")).get

    val maleModelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/male.json")).get.filter(p => !p.id.contains("radial")).sortBy{lm => lm.id}
    val femaleModelLm = LandmarkIO.readLandmarksJson[_3D](new File("data/fbm-landmarks/female.json")).get.filter(p => !p.id.contains("radial")).sortBy{lm => lm.id}

    val augMaleModel = marginalModelArmCut(augmentFittingModel(maleModel, 50, 200))
    val augFemaleModel = augmentFittingModel(femaleModel, 0.005, 20.0)

    val maleModelContainer = modelContainer(augMaleModel, maleModelLm)
    val femaleModelContainer = modelContainer(augFemaleModel, femaleModelLm)

    val meshLm = LandmarkIO.readLandmarksJson[_3D](new File("data/mpi-training/mpi.json")).get.sortBy{lm => lm.id}
    val meshes = new File("data/mpi-training/").listFiles.filter{f => f.getName.contains("stl") && !f.getName.contains("reference")}.sortBy{f => f.getName}.take(2).map{f => MeshIO.readMesh(f).get}

    val meshReference = MeshIO.readMesh(new File("data/mpi-training/reference.stl")).get
    val meshWaistAntID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.anterior").head.point).id
    val meshWaistPostID = meshReference.pointSet.findClosestPoint(meshLm.filter(p => p.id == "waist.posterior").head.point).id

    val realHeight: IndexedSeq[Double] = meshes.map{mesh => Measurement.measurePointHeight(mesh)}//Source.fromFile("data/image-landmarks/height.txt").getLines().toIndexedSeq.map{h => h.toDouble}
    val realWC: IndexedSeq[Double] = meshes.map{mesh => Measurement.getWaistCircumference(mesh, mesh.pointSet.point(meshWaistAntID), mesh.pointSet.point(meshWaistPostID)) * 1e3}
    val realVol: IndexedSeq[Double] = meshes.map{mesh => Measurement.getMeshVolume(mesh) * 1e3}

    val sexFile = Source.fromFile("data/image-landmarks/sex.txt")
    val sex: IndexedSeq[String] = sexFile.getLines().toIndexedSeq
    sexFile.close()
    val tlmsLandmarks = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("tlms")}.sortBy{f => f.getName}
    val imageFiles = new File("data/image-landmarks/").listFiles.filter{f => f.getName.contains("png")}.sortBy{f => f.getName}

    val imgLmFront: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val frontImages = imageFiles.filter{f => f.getName.contains("Front")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val targetLmFront: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmFront.indices.map{i =>
      val lmHeightFront = imgLmFront(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmFront(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i) * 1e3
      val image = frontImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightFront)
      // Scale image. Scale to correct distance and coordinate system
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmFront(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    val sideImages = imageFiles.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => ImageIO.read(f)}
    val imgLmSide: IndexedSeq[IndexedSeq[TLMSLandmark2D]] = tlmsLandmarks.filter{f => f.getName.contains("Side")}.sortBy{f => f.getName}.map{f => TLMSLandmarksIO.read2D(f).get}.toIndexedSeq
    val targetLmSide: IndexedSeq[IndexedSeq[Landmark[_2D]]] = imgLmSide.indices.map{i =>
      val lmHeightSide = imgLmSide(i).filter{p => p.id == "metatarsal-phalangeal.v.rt"}.head.point.y - imgLmSide(i).filter{p => p.id == "crown"}.head.point.y
      val height = realHeight(i)
      val image = sideImages(i)
      val scaleFactor = imageScalingFactor(height, image.getHeight, lmHeightSide)
      val targetFront: IndexedSeq[Landmark[_2D]] = imgLmSide(i).map{l =>
        val newPoint = EuclideanVector(l.point.x * scaleFactor, l.point.y * scaleFactor).toPoint
        new Landmark[_2D](l.id, newPoint)
      }
      targetFront.sortBy(l => l.id)
    }

    // Load data into container object
    val targets = targetLmFront.indices.map{i =>
      var fittingModelContainer: fittingModelContainer = null
      if (sexEnum.withNameWithDefault(sex(i)) == sexEnum.MALE){
        val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, 0.045, 0.001)
        fittingModelContainer = new fittingModelContainer(fitData, maleModelContainer)
      }
      else {
        val fitData = fittingData(targetLmFront(i), targetLmSide(i).filter{lm => lm.id.contains("waist") || lm.id.contains("thelion")}, 0.5, 0.05)
        fittingModelContainer = new fittingModelContainer(fitData, femaleModelContainer)
      }

      fittingModelContainer
    }

    //println("Data loaded. Starting fitting")

    val results: IndexedSeq[(TriangleMesh[_3D], Double, Double, Double)] = targets.map{target =>
      val posterior = landmarkPosterior(target)
      val waistAntID = target.modelCon.model.referenceMesh.pointSet.findClosestPoint(target.modelCon.modelLM.filter(p => p.id == "waist.anterior").head.point).id
      val waistPostID = target.modelCon.model.referenceMesh.pointSet.findClosestPoint(target.modelCon.modelLM.filter(p => p.id == "waist.posterior").head.point).id
      val bestFit = posterior.mean
      val heightPosterior = Measurement.measurePointHeight(bestFit)
      val wcPosterior = Measurement.getWaistCircumference(bestFit, bestFit.pointSet.point(waistAntID), bestFit.pointSet.point(waistPostID)) * 1e+3
      val volPosterior = Measurement.getMeshVolume(bestFit) * 1e+3

      val result: (TriangleMesh[_3D], Double, Double, Double) = (bestFit, heightPosterior, wcPosterior, volPosterior)
      result
    }
    val ui = ScalismoUI()
    val resultGroups = IndexedSeq[Group](ui.createGroup("Male"), ui.createGroup("Female"))

    results.indices.map{i =>
      val original = meshes(i)
      val result = results(i)._1

      val meshLms = meshLm.map{lm => new Landmark[_3D](lm.id, original.pointSet.point(meshReference.pointSet.findClosestPoint(lm.point).id))}
      val resultLm = targets(i).modelCon.modelLM.map{lm => new Landmark[_3D](lm.id, result.pointSet.point(targets(i).modelCon.model.referenceMesh.pointSet.findClosestPoint(lm.point).id))}

      val cutResult = modelMeshArmClip(result)

      val alignedOriginal = original.transform(LandmarkRegistration.rigid3DLandmarkRegistration(meshLms, resultLm, computeCentreOfMass(result)))
      val alignedCutOriginal = mpiFingerClip(alignedOriginal)

      println("Average Distance")
      println(MeshMetrics.avgDistance(cutResult, alignedCutOriginal))
      println("Hausdorff Distance")
      println(Hausdorff.modifiedHausdorffDistance(cutResult, alignedCutOriginal))

      ui.show(resultGroups(i), cutResult, "Fitted: " + i.toString)
      ui.show(resultGroups(i), alignedCutOriginal, "Original: " + i.toString)
    }

    val bestFitHeightPosterior = results.map{result => result._2}
    val bestFitWCPosterior = results.map{result => result._3}
    val bodyVolPosterior = results.map{result => result._4}
    println("Best Fit Height vs Real Height")
    println(bestFitHeightPosterior, realHeight)
    println("Best Fit WC vs Measured WC")
    println(bestFitWCPosterior, realWC)
    println("Best Fit Vol vs Measured Vol")
    println(bodyVolPosterior, realVol)
  }

  def landmarkPosterior(fittingModel: fittingModelContainer): StatisticalMeshModel = {

    val model = fittingModel.modelCon.model
    val modelLm = fittingModel.modelCon.modelLM
    val targetLmFront = fittingModel.fitData.lmFront
    val targetLmSide = fittingModel.fitData.lmSide
    val frontNoise = fittingModel.fitData.frontNoise
    val sideNoise = fittingModel.fitData.sideNoise

    val targetLmFront3D = targetLmFront.map{l => new Landmark[_3D](l.id, Point3D(l.point.x, l.point.y, 0.0))}

    val targetLmSide3D = targetLmSide.map{l => new Landmark[_3D](l.id, Point3D(0.0, l.point.y, l.point.x))}

    val modelLmFront = modelLm.filter{l =>
      val filtered = targetLmFront.filter(_.id == l.id)
      filtered.nonEmpty
    }.sortBy(l => l.id)

    val modelLmSide = modelLm.filter{l =>
      val filtered = targetLmSide.filter(_.id == l.id)
      filtered.nonEmpty
    }.sortBy(l => l.id)

    val littleNoiseFront = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * frontNoise)
    littleNoiseFront.cov.data(8) = Double.MaxValue

    val referenceMesh = model.referenceMesh
    val referencePIDFront = modelLmFront.map{pt => referenceMesh.pointSet.findClosestPoint(pt.point).id}

    val rigidTransFront = LandmarkRegistration.rigid3DLandmarkRegistration(targetLmFront3D, modelLmFront, computeCentreOfMass(model.referenceMesh))
    val targetFrontTransformed = targetLmFront3D.map{t => t.transform(rigidTransFront).point}
    val regressionDataFront = for ((refPointId, targetPoint) <- referencePIDFront zip targetFrontTransformed) yield {
      (refPointId, targetPoint, littleNoiseFront)
    }

    val posteriorFront = model.posterior(regressionDataFront.toIndexedSeq)

    val littleNoiseSide = MultivariateNormalDistribution(DenseVector.zeros[Double](3), DenseMatrix.eye[Double](3) * sideNoise)
    littleNoiseSide.cov.data(0) = Double.MaxValue

    val posteriorLmSide = modelLmSide.map{lm => new Landmark[_3D](lm.id, posteriorFront.referenceMesh.pointSet.point(model.referenceMesh.pointSet.findClosestPoint(lm.point).id))}
    val referencePIDSide = posteriorLmSide.map{pt => posteriorFront.referenceMesh.pointSet.findClosestPoint(pt.point).id}

    val rigidTransSide = LandmarkRegistration.rigid3DLandmarkRegistration(targetLmSide3D, posteriorLmSide, computeCentreOfMass(posteriorFront.referenceMesh))
    val targetSideTransformed = targetLmSide3D.map{t => t.transform(rigidTransSide).point}
    val regressionDataSide = for ((refPointId, targetPoint) <- referencePIDSide zip targetSideTransformed) yield {
      (refPointId, targetPoint, littleNoiseSide)
    }

    val posteriorSide = posteriorFront.posterior(regressionDataSide.toIndexedSeq)

    posteriorSide
  }

  /**
    * Removes forearm from statistical mesh model generataed mesh
    *
    * Parameters:
    *    -   `mesh` Mesh to be clipped
    * Returns:
    *    -  Clipped mesh
    */
  def modelMeshArmClip(mesh: TriangleMesh[_3D]): TriangleMesh[_3D] = {
    val file = Source.fromFile("data/fbm-landmarks/armid.txt")
    val pid: IndexedSeq[Int] = file.getLines().map{id => id.toInt}.toIndexedSeq
    file.close()
    val points = pid.map{id => mesh.pointSet.point(PointId(id))}
    mesh.operations.clip(p => {points.contains(p)})
  }

  /**
    * Removes forearm from MPI mesh
    *
    * Parameters:
    *    -   `mesh` MPI mesh to be clipped
    * Returns:
    *    -  Clipped mesh
    */
  def mpiFingerClip(mesh: TriangleMesh[_3D]): TriangleMesh[_3D] = {
    val file = Source.fromFile("data/mpi-training/mpiArm.txt")
    val pid: IndexedSeq[Int] = file.getLines().map{id => id.toInt}.toIndexedSeq
    file.close()
    // To account for missing vertices, cross filter first before clipping
    val meshSeq = mesh.pointSet.pointIds.toIndexedSeq.map{f => f.id}
    val points = pid.filter{id =>
      val filtered = meshSeq.filter(_ == id)
      filtered.nonEmpty
    }
    mesh.operations.clip(p => {points.contains(mesh.pointSet.pointId(p).get.id)})
  }

  /**
    * Create marginal shape model without arms
    *
    * Parameters:
    *    -   `model` Statistical mesh model to marginalise
    * Returns:
    *    -  Marginal statistical mesh model
    */
  def marginalModelArmCut(model: StatisticalMeshModel): StatisticalMeshModel = {
    val file = Source.fromFile("data/fbm-landmarks/armid.txt")
    val pid = file.getLines().map{id => id.toInt}.toIndexedSeq.map{p => PointId(p)}
    val points = model.referenceMesh.pointSet.points.toIndexedSeq.map{p => model.referenceMesh.pointSet.pointId(p).get}.filter(p => !pid.contains(p))
    file.close()
    model.marginal(points)
  }

  /**
    * Calculates an image scaling factor based on camera parameters. What if object was certain distance from camera
    *
    * Parameters:
    *    -   `realHeight` Real height of the object (mm)
    *    -   `imageHeight` Height of the image (pixels)
    *    -   `objectHeight` Height of the object (pixels)
    *    -   `focalLength` Focal length of camera (mm)
    *    -   `sensorHeight` Height of camera sensor (mm)
    *    -   `subjectDistance` Distance of subject (mm)
    * Returns:
    *    -  Image scaling factor
    */
  def imageScalingFactor(realHeight: Double, imageHeight: Int, objectHeight: Double, focalLength: Double = 3.8, sensorHeight: Double = 8.3, subjectDistance: Double = 1000.0): Double = {
    val scaledObjectHeight = focalLength * realHeight * imageHeight/(sensorHeight * subjectDistance)
    val scalingFactor = scaledObjectHeight/objectHeight
    val dimensionScaler = realHeight/(1000 * scaledObjectHeight)
    scalingFactor * dimensionScaler
  }

  /**
    * Computes mesh centre of mass
    *
    * Parameters:
    *    -   `mesh` Mesh to have centre of mass calculated
    * Returns:
    *    -  Centre of mass point
    */
  def computeCentreOfMass(mesh: TriangleMesh[_3D]): Point[_3D] = {
    val normFactor = 1.0 / mesh.pointSet.numberOfPoints
    mesh.pointSet.points.foldLeft(Point(0, 0, 0))((sum, point) => sum + point.toVector * normFactor)
  }

  def augmentFittingModel (model: StatisticalMeshModel, l: Double, sig: Double): StatisticalMeshModel = {
    val scalarValuedKernel = GaussianKernel[_3D](sig) * l

    case class XmirroredKernel(kernel : PDKernel[_3D]) extends PDKernel[_3D] {
      override def domain = RealSpace[_3D]
      override def k(x: Point[_3D], y: Point[_3D]) = kernel(Point(x(0) * -1f ,x(1), x(2)), y)
    }

    def symmetrizeKernel(kernel : PDKernel[_3D]) : MatrixValuedPDKernel[_3D] = {
      val xmirrored = XmirroredKernel(kernel)
      val k1 = DiagonalKernel(kernel, 3)
      val k2 = DiagonalKernel(xmirrored * -1f, xmirrored, xmirrored)
      k1 + k2
    }

    val gp = GaussianProcess[_3D, EuclideanVector[_3D]](symmetrizeKernel(scalarValuedKernel))

    val sampler = RandomMeshSampler3D(
      model.referenceMesh,
      numberOfPoints = 800,
      seed = 0
    )
    val lowRankGP = LowRankGaussianProcess.approximateGPNystrom(
      gp,
      sampler,
      numBasisFunctions = 200
    )(ThreeDSpace, vectorizer = gp.vectorizer)

    StatisticalMeshModel.augmentModel(model, lowRankGP)
  }

  object orientationEnum extends Enumeration {
    type orientationEnum = Value
    val FRONT, SIDE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      orientationEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
  }

  private case class fittingData(lmFront: IndexedSeq[Landmark[_2D]], lmSide: IndexedSeq[Landmark[_2D]], frontNoise: Double, sideNoise: Double){
    def getLmFront: IndexedSeq[Landmark[_2D]] = lmFront
    def getLmSide: IndexedSeq[Landmark[_2D]] = lmSide
  }

  private case class modelContainer(model: StatisticalMeshModel, modelLM: Seq[Landmark[_3D]]){
    def getModel: StatisticalMeshModel = model
    def getLM: Seq[Landmark[_3D]] = modelLM
  }

  private case class fittingModelContainer(fitData: fittingData, modelCon: modelContainer){
    def getData: fittingData = fitData
    def getModel: modelContainer = modelCon
  }

}