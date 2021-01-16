package observatory

import java.time.LocalDate
import scala.collection.parallel.ParSeq
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  // some utility case classes
  case class StationLine( stn: String, wban: String, lat:String, lon:String )
  case class TemperatureLine( stn: String, wban: String, month:String, day:String, temperature:String )
  case class TemperatureReading( stationId: (String,String), date: LocalDate, temperature: Temperature )

  /**
    * @author acdhirr
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val sLines: Seq[String] = Source.fromInputStream(getClass.getResourceAsStream(s"$stationsFile"), "utf-8")
      .getLines().toSeq

    val tLines: Seq[String] = Source.fromInputStream(getClass.getResourceAsStream(s"$temperaturesFile"), "utf-8")
      .getLines().toSeq

    locateTemperaturesFromLists(year, sLines, tLines)
  }

  /**
    * @author acdhirr
    * @param year     year
    * @param sLines   Seq of Strings (station csv lines)
    * @param tLines   Seq of Strings (temperature csv lines)
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperaturesFromLists(year: Year, sLines: Seq[String], tLines: Seq[String]): Iterable[(LocalDate, Location, Temperature)] = {

    // 1: read in the station lines
    val stationLines = parseStationLines(sLines)
    // 2: read in the temperature lines
    val temperatureLines = parseTemperatureLines(tLines)

    // temperatureLines.foreach(println)

    // convert temperature lines to temperature readings
    val temperatureReadings = getTemperatureReadings(temperatureLines,year)

    // convert station lines to a lookup table (dictionary) for stations
    val stationsMap = getStationsMap(stationLines)

    // join temperature readings with stations
    // (ignore readings for stations that have no, or an invalid location defined)
    temperatureReadings
      .filter(temp => stationsMap.isDefinedAt(temp.stationId))
      .map(reading => (reading.date, stationsMap(reading.stationId), reading.temperature))
      .seq
  }

  /**
    * @author acdhirr
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    // helper function to get average temperature
    def average(list: Seq[(LocalDate, Location, Temperature)]): Temperature =
      list.map(t => t._3).sum / list.size

    // group by location
    val q = records.toSeq.groupBy{
      case(date, loc, temp) => loc
    }
    // then get average
    .par.mapValues((v: Seq[(LocalDate, Location, Temperature)]) => average(v))

    q.seq
  }

  /**
    * @author acdhirr
    * @param tempFahrenheit temperature in Fahrenheit
    * @return temperature in Celsius
    */
  def fahrenheit2Celsius(tempFahrenheit: Double): Temperature =
    (tempFahrenheit - 32) / 1.8000

  /**
    * @author acdhirr
    * @param lines
    * @return Seq of StationLine (csv Strings)
    */
  def parseStationLines(lines: Seq[String]): ParSeq[StationLine] =

    lines.par.map(line => {
      val ar: Array[String] = line.split(",", 4)
      StationLine(ar(0),ar(1),ar(2),ar(3))
    })
    // stations with no location are ignored
    .filterNot(line => line.lat.isEmpty || line.lon.isEmpty)
    // stations with (0,0) location are ignored
    .filterNot(line => line.lat.equals("+00.000") && line.lon.equals("+000.000"))
    // stations with no ids are ignored
    .filterNot(line => line.stn.isEmpty && line.wban.isEmpty)

  /**
    * @author acdhirr
    * @param lines
    * @return Seq of TemperatureLine (csv Strings)
    */
  def parseTemperatureLines(lines: Seq[String]): ParSeq[TemperatureLine] =

    lines.par.map(line => {
      val ar: Array[String] = line.split(",", 5)
      TemperatureLine(ar(0),ar(1),ar(2),ar(3),ar(4))
    })
    .filterNot(_.temperature.equals("9999.9"))
    .filterNot(t => t.stn.isEmpty && t.wban.isEmpty)


  /**
    * @author acdhirr
    * @param stationLines
    * @return Map (stn,wban) stationId to Location
    */
  def getStationsMap(stationLines: ParSeq[StationLine]) =

    stationLines
      .map(line => {
        (
          (line.stn,line.wban),  // id consists of STN + WBAN id
          Location( line.lat.toDouble, line.lon.toDouble )
        )
      }).toMap

  /**
    * @author acdhirr
    * @param temperatureLines
    * @param year
    * @return Seq of TemperatureReading (stationId, date, temperature)
    */
  def getTemperatureReadings(temperatureLines: ParSeq[TemperatureLine], year: Int): ParSeq[TemperatureReading] =

    temperatureLines.par.map(line => {
      TemperatureReading(
        (line.stn,line.wban),  // id consists of STN + WBAN id
        LocalDate.of(year, line.month.toInt, line.day.toInt), // date
        fahrenheit2Celsius( line.temperature.toDouble ) // temperature in C
      )
    })


}
