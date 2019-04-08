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
    var  sqVol = new ListBuffer[Double]

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
    val arDataset = arFiles.map{f => MeshIO.readMesh(f).get}
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
}