package tools

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.util
import java.util.Calendar

import com.opencsv.CSVWriter
import scalismo.faces.landmarks.TLMSLandmark2D
import scalismo.geometry.{Landmark, Point, EuclideanVector, _3D}
import scalismo.mesh.TriangleMesh
import scalismo.statisticalmodel.StatisticalMeshModel

import scala.collection.mutable.ListBuffer

object Utils {

  def gpVaryingRank(gpModel: StatisticalMeshModel, components: Int): Seq[StatisticalMeshModel] = {

    val gpModels = for(i <- 1 to components) yield {
      gpModel.truncate(i)
    }
    gpModels
  }

  def writeToFile(matrix: Seq[Seq[Double]], outputPath: String): Unit = {

    val writer = openFile(outputPath)

    for(j <- matrix.indices) yield {

      writer.write(j + " PCA components"+"\r\n")
      for(i<- matrix(j).indices) {
        writer.write(matrix(j)(i).toString+" ")

      }

      writer.write("\r\n")
    }
    writeMean(matrix, writer)
    writer.close()
  }

  def openFile(outputPath: String): PrintWriter = {
    val writer = new PrintWriter(new File(outputPath))
    val today = Calendar.getInstance().getTime
    writer.write(today+"\r\n"+"\r\n")
    writer
  }

  def writeMean(matrix: Seq[Seq[Double]], writer: PrintWriter): Unit = {

    val toCurve : IndexedSeq[(Double,Double)] = toAvgSD(matrix)
    writer.write("\r\n\r\nAveraged values to be displayed...\r\n")
    writer.write("    mean:\r\n")

    for(j <- toCurve.indices) {

      val (mean,_) = toCurve(j)
      writer.write(mean.toString+" ")

    }

    writer.write("\r\n    stdDev:\r\n")

    for(j <- toCurve.indices) {

      val (_,sd) = toCurve(j)
      writer.write(sd.toString+" ")

    }
  }

  // Returns a table containing mean and standard deviation as a function of the number of PCA modes
  def toAvgSD(crossValidation: Seq[Seq[Double]]): IndexedSeq[(Double, Double)] = {

    val tab : IndexedSeq[(Double, Double)] = for (j <- crossValidation.indices) yield {

      val mean : Double = crossValidation(j).sum / crossValidation(j).length
      val devs = for (cv<- crossValidation(j)) yield {(cv - mean) * (cv - mean)}
      val stddev : Double = Math.sqrt(devs.sum / crossValidation(j).length)

      (mean, stddev)
    }

    tab
  }

  def csvWrite(csvSchema: Seq[String], data: Seq[Seq[Any]], outputDirectory: String): Unit = {

    val outputFile = new BufferedWriter(new FileWriter(outputDirectory))
    val csvWriter = new CSVWriter(outputFile)

    val listOfRecords = new util.ArrayList[Array[String]]()
    listOfRecords.add(csvSchema.toArray)

    for (i <- data.head.indices){
      val row = data.map{f =>
        f.seq(i).toString
      }
      listOfRecords.add(row.toArray)
    }

    csvWriter.writeAll(listOfRecords)

    outputFile.close()
  }

  def getDifferences(reference: IndexedSeq[Double], test: IndexedSeq[Double]): (Double, Double) = {
    val difference = reference.indices.map{i =>
      reference(i) - test(i)
    }

    val meanDifference = difference.sum/(difference.size * 1.0)
    val absDifference = difference.map{diff => math.abs(diff)}.sum/(difference.size * 1.0)

    (meanDifference, absDifference)
  }

  object sexEnum extends Enumeration {
    type sexEnum = Value
    val FEMALE, MALE, UNKNOWN = Value

    def withNameWithDefault(s: String): Value =
      sexEnum.values.find(_.toString.toLowerCase == s.toLowerCase()).getOrElse(UNKNOWN)
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

  def landmarkAverage(landmarks: IndexedSeq[IndexedSeq[IndexedSeq[IndexedSeq[Landmark[_3D]]]]]): IndexedSeq[IndexedSeq[Landmark[_3D]]] = {
    val avePersonLandmark = landmarks.map{person =>
      person(0).indices.map{i =>
        person(0)(i).indices.map{j =>
          val id = person(0)(i)(j).id
          val pointA = person(0)(i)(j).point
          val pointB = person(1)(i)(j).point
          val newPoint = EuclideanVector((pointA.x + pointB.x)/2.0, (pointA.y + pointB.y)/2.0, (pointA.z + pointB.z)/2.0).toPoint
          val description = person(0)(i)(j).description
          val uncertainty = person(0)(i)(j).uncertainty
          new Landmark[_3D](id, newPoint, description, uncertainty)
        }
      }
    }
/*
    val fields = ListBuffer("Landmark", "x", "y", "z")
    val directory = "data/landmark-test/"

    landmarks.indices.map{k =>
      val person = landmarks(k)
      person.indices.map{i =>
        person(i).indices.foreach{j =>
          var data = new ListBuffer[Seq[Any]]
          val landmarkSet = person(i)(j).sortBy(_.id)
          val names = landmarkSet.map{_.id}
          val x = landmarkSet.map{_.point.x}
          val y = landmarkSet.map{_.point.y}
          val z = landmarkSet.map{_.point.z}
          data += names
          data += x
          data += y
          data += z
          csvWrite(fields, data.toList, directory + "people-average/" + k.toString + "/" + i.toString + "/" + j.toString + ".csv")
        }
      }
    }

    avePersonLandmark.indices.foreach{i =>
      val person = avePersonLandmark(i)
      person.indices.foreach{j =>
        var data = new ListBuffer[Seq[Any]]
        val landmarkSet = person(j).sortBy(_.id)
        val names = landmarkSet.map{_.id}
        val x = landmarkSet.map{_.point.x}
        val y = landmarkSet.map{_.point.y}
        val z = landmarkSet.map{_.point.z}
        data += names
        data += x
        data += y
        data += z
        csvWrite(fields, data.toList, directory + "global-average/" + i.toString + "/" + j.toString + ".csv")
      }
    }*/
    // Average person landmarks calculated
    // Now calculate average people landmarks

    val aveLandmarks = avePersonLandmark(0).indices.map{i =>
      avePersonLandmark(0)(i).indices.map{j =>
        val id = avePersonLandmark(0)(i)(j).id
        val pointA = avePersonLandmark(0)(i)(j).point
        val pointB = avePersonLandmark(1)(i)(j).point
        val newPoint = EuclideanVector((pointA.x + pointB.x)/2.0, (pointA.y + pointB.y)/2.0, (pointA.z + pointB.z)/2.0).toPoint
        val description = avePersonLandmark(0)(i)(j).description
        val uncertainty = avePersonLandmark(0)(i)(j).uncertainty
        new Landmark[_3D](id, newPoint, description, uncertainty)
        }
      }
/*
    aveLandmarks.indices.foreach{i =>
      val landmark = aveLandmarks(i).sortBy(_.id)
      var data = new ListBuffer[Seq[Any]]
      val names = landmark.map{_.id}
      val x = landmark.map{_.point.x}
      val y = landmark.map{_.point.y}
      val z = landmark.map{_.point.z}
      data += names
      data += x
      data += y
      data += z
      csvWrite(fields, data.toList, directory + "the-average/ave.csv")
    }
*/
    aveLandmarks
    }

  def imageLandmarkAverage(landmarks: IndexedSeq[IndexedSeq[IndexedSeq[IndexedSeq[TLMSLandmark2D]]]]): IndexedSeq[IndexedSeq[TLMSLandmark2D]] = {
    val avePersonLandmark = landmarks.map{person =>
      person(0).indices.map{i =>
        person(0)(i).indices.map{j =>
          val id = person(0)(i)(j).id
          val pointA = person(0)(i)(j).point
          val pointB = person(1)(i)(j).point
          val newPoint = EuclideanVector((pointA.x + pointB.x)/2.0, (pointA.y + pointB.y)/2.0).toPoint
          TLMSLandmark2D(id, newPoint, visible = true)
        }
      }
    }
/*
    val fields = ListBuffer("Landmark", "x", "y")
    val directory = "data/landmark-image-test/"

    landmarks.indices.map{k =>
      val person = landmarks(k)
      person.indices.map{i =>
        person(i).indices.foreach{j =>
          var data = new ListBuffer[Seq[Any]]
          val landmarkSet = person(i)(j).sortBy(_.id)
          val names = landmarkSet.map{_.id}
          val x = landmarkSet.map{_.point.x}
          val y = landmarkSet.map{_.point.y}
          data += names
          data += x
          data += y
          csvWrite(fields, data.toList, directory + "people-average/" + k.toString + "/" + i.toString + "/" + j.toString + ".csv")
        }
      }
    }

    avePersonLandmark.indices.foreach{i =>
      val person = avePersonLandmark(i)
      person.indices.foreach{j =>
        var data = new ListBuffer[Seq[Any]]
        val landmarkSet = person(j).sortBy(_.id)
        val names = landmarkSet.map{_.id}
        val x = landmarkSet.map{_.point.x}
        val y = landmarkSet.map{_.point.y}
        data += names
        data += x
        data += y
        csvWrite(fields, data.toList, directory + "global-average/" + i.toString + "/" + j.toString + ".csv")
      }
    }
*/
    // Average person landmarks calculated
    // Now calculate average people landmarks

    val aveLandmarks = avePersonLandmark(0).indices.map{i =>
      avePersonLandmark(0)(i).indices.map{j =>
        val id = avePersonLandmark(0)(i)(j).id
        val pointA = avePersonLandmark(0)(i)(j).point
        val pointB = avePersonLandmark(1)(i)(j).point
        val newPoint = EuclideanVector((pointA.x + pointB.x)/2.0, (pointA.y + pointB.y)/2.0).toPoint
        TLMSLandmark2D(id, newPoint, visible = true)
      }
    }
/*
    aveLandmarks.indices.foreach{i =>
      val landmark = aveLandmarks(i).sortBy(_.id)
      var data = new ListBuffer[Seq[Any]]
      val names = landmark.map{_.id}
      val x = landmark.map{_.point.x}
      val y = landmark.map{_.point.y}
      data += names
      data += x
      data += y
      csvWrite(fields, data.toList, directory + "the-average/ave.csv")
    }*/
    aveLandmarks
  }

}
