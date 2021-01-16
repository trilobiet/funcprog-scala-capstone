package observatory

import java.time.LocalDate

object Main extends App {

  val years = (1982 to 2015).toList

  years.foreach( year => {
    val measurements: Iterable[(LocalDate, Location, Temperature)] =
      Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")

    val avgs = Extraction.locationYearlyAverageRecords(measurements)
    println("Measurements: --------------------------")
    measurements.toSeq.take(100).foreach(println)
    println("Averages: --------------------------")
    avgs.toSeq.take(100).foreach(println)
  })
}
