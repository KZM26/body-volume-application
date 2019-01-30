package preprocessor

import scalismo.io.MeshIO
import java.io.File

import scalismo.ui.api.ScalismoUI

class preprocessor {

  def start(): Unit = {

    val preprocessorConfig = List("Landmark reference shape (l)", "Align to Ref (a)", "Help (h)", "Quit (q)\n")

    var input = ""

    while (input != "q") {
      input = scala.io.StdIn.readLine(preprocessorConfig.mkString("\n"))
      input match {

        case "l" => // Start landmarking
          this.landmarkRef()

        case "a" => // Start Alignment
        // TODO

        case "h" => // Help
          println("Learn how to use a computer you scrub\n")

        case "q" => // Quit
          input = scala.io.StdIn.readLine("Are you sure you want to quit (y/n)?\n")
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

    def landmarkRef(): Unit = {

      // Initialise Scalismo
      scalismo.initialize()

      println("Scalismo Viewer will now open\nSelect the landmarks and save them")

      // create a visualisation window
      val ui = ScalismoUI()

      // Load the reference shape
      val mesh = MeshIO.readMesh(new File("data/training/tr_gt_000.stl")).get

      // Display it
      val meshView = ui.show(mesh, "reference")

      // val files = new File("data/training/").listFiles
      // val dataset = files.map{f => MeshIO.readMesh(f).get}
      // (0 until dataset.size).foreach{i => show(dataset(i),"body_"+i)}

    }

}
