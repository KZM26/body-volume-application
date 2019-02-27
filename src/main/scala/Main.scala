// Packages
import preprocessor.Preprocessor
import build.Build
import measurement.Measurement

// Main
object Main extends App{
    val baseConfig = List("Build (b)", "Fitting (f)", "Measurements (m)", "Preprocessor (p)", "Help (h)", "Quit (q)\n")
    val pConfig = List("View Reference Shape (v)", "Align to Reference (a)")

    println("Welcome to the Body Volume Application\nSelect an option")

    var input = ""

    while (input != "q"){

      input = scala.io.StdIn.readLine(baseConfig.mkString("\n")).toLowerCase()
      input match{

          case "b" => // Buildz`
            val build = new Build
            build.start()

          case "f" => // Fitting
            // TODO

          case "m" => // Measurement
            val measure = new Measurement
            measure.start()

          case "p" => // Preprocessor
            val processor = new Preprocessor
            processor.start()

          case "h" => // Help
            println("Learn how to use a computer you scrub\n")

          case "q" => // Quit
            input = scala.io.StdIn.readLine("Are you sure you want to quit (y/n)?\n").toLowerCase()
            input match{

            case "y" => // Yes
              sys.exit(0)

              case _ => // Any
                println("That ain't it chief\n")
              }

          case _ => // Any
             println("That ain't it chief\n")
        }
        input = scala.io.StdIn.readLine(baseConfig.mkString("\n")).toLowerCase()
    }

}