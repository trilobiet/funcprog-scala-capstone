package observatory

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter

import java.time.LocalDate

object Main extends App {

  val year = 2015
  val colorScale: Map[Temperature,Color] = Map(
    60d	-> Color(255,255,255),
    32d	-> Color(255,0,0),
    12d	-> Color(255,255,0),
    0d	-> Color(0,255,255),
    -15d	-> Color(0,0,255),
    -27d	-> Color(255,0,255),
    -50d	-> Color(33,0,107),
    -60d	-> Color(0,0,0)
  )

  val measurements: Iterable[(LocalDate, Location, Temperature)] =
    Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")

  val averages: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecords(measurements)

  println("Averages: " + averages.size)

  val img: Image = Visualization.visualize(averages,colorScale)

  implicit val writer = PngWriter.NoCompression
  img.output(new java.io.File("target/some-image.png"))



  val result: (Int, Int) =
    List(1,2,3,4,5,6,7,8,9,10).par.aggregate(
      (0, 0)  // acc start value
    )(
      (acc: (Int, Int), y:Int) => (acc._1 + y, acc._2 + 1),  // acc left holds sum, acc right counts
      (a: (Int, Int), b: (Int, Int)) =>(a._1 + b._1, a._2 + b._2)  // combine accumulators
    )

}

