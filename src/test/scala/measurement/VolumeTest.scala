package measurement

import java.io.File

import measurement.Measurement.getMeshVolume
import test.BVATestSuite
import scalismo.io.MeshIO

import scala.collection.mutable.ListBuffer
import scala.io.Source

class VolumeTest extends BVATestSuite {
  it("Cube volume measurements exactly equal") {

    val files = new File("data/distance-test/").listFiles
    val sqFiles = files.filter(f => f.getName.contains("sq")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val sqTrueVolume = sqFiles.map{f => math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val sqDataset = sqFiles.map{f => MeshIO.readMesh(f).get}

    for (i <- sqDataset.indices){
      val mesh = sqDataset(i)
      assert(getMeshVolume(mesh) == sqTrueVolume(i))
    }
  }

  it ("Sphere volume within 99% of expected value"){

    val spFiles = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("sp")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val spTrueVolume =  spFiles.map{f => (4.0/3.0) * math.Pi * math.pow(f.getName.substring(0, f.getName.indexOf(".") - 2).toInt, 3)}
    val spDataset = spFiles.filter(f => f.getName.contains(".stl")).map{f => MeshIO.readMesh(f).get}

    for (i <- spDataset.indices){
      val mesh = spDataset(i)
      assert(math.abs(getMeshVolume(mesh)/spTrueVolume(i) - 1) < 0.01)
    }
  }

  it ("Arbitrary shape volume within 95%"){

    val arFiles = new File("data/volume-test/").listFiles.filter(f => f.getName.contains("shape")).filter(f => f.getName.contains(".stl")).sortBy(_.getName)
    val volFile = Source.fromFile("data/volume-test/volume.txt")
    // Read line by line using iterator. Drop first two lines
    val iter = volFile.getLines().toIndexedSeq

    // Extract shape name and volume line by line
    // Line Format: name-volume
    iter.map{ info =>
      val shapeName = info.substring(0, info.indexOf("-"))
      val shapeVolume = info.substring(info.indexOf("-") + 1, info.length).toDouble
      val mesh = MeshIO.readMesh(arFiles.filter(p => p.getName.containsSlice(shapeName)).head).get
      assert(math.abs(getMeshVolume(mesh)/shapeVolume - 1) < 0.05)
    }
  }

  it ("Body volume within 95%"){

    val bodyFiles = new File("data/inkreate/").listFiles.sortBy(f => f.getName)
    val bodyDataset = bodyFiles.map{f => MeshIO.readMesh(f).get}

    // Read out the same number of values from csv as there are body files
    // Read file
    val csvSRC = Source.fromFile("data/inkreate-ref/measurements.csv")
    // Read line by line using iterator. Drop first two lines
    val lines = csvSRC.getLines().drop(2).map(_.split(",")).toIndexedSeq
    val csvVolume = lines.map{row =>
      // Extract each volume (in column 111 = CY)
      // Multiple to convert to mm3 to match getMeshVolume output unit
      row(102).toDouble * 1e+6
    }

    bodyDataset.map{mesh =>
      assert(math.abs(getMeshVolume(mesh)/csvVolume(bodyDataset.indexOf(mesh)) - 1) < 0.05)
    }
  }
}