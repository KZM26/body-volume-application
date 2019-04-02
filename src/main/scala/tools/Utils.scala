package tools

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.util
import java.util.Calendar

import au.com.bytecode.opencsv.CSVWriter
import scalismo.statisticalmodel.StatisticalMeshModel

object Utils {

  def gpVaryingRank(gpModel: StatisticalMeshModel, components: Int): Seq[StatisticalMeshModel] = {

    val gpModels = for(i<-1 to components) yield {
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

}
