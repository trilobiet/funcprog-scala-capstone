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
  img.output(new java.io.File(s"target/map-$year.png"))

}

