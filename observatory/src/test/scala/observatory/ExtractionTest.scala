package observatory

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {

  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  val csv2021 =
    """
    |010010,,01,01,35.9
    |010010,,01,02,34.7
    |010010,,01,03,35.3
    |010010,,01,04,38.2
    |010010,,01,05,35.9
    |010010,,01,06,34.5
    |010010,,01,07,32.0
    |010010,,01,08,31.7
    |,,01,08,31.7
    |720609,00201,01,01,25.0
    |720609,00201,02,02,25.0
    |720609,00201,03,03,25.0
    |720609,00201,04,04,25.0
    |720609,00201,05,05,25.0
    |720609,00201,06,06,26.0
    |720609,00201,07,07,26.0
    |720609,00201,08,08,26.0
    |720609,00201,09,09,26.0
    |720609,00201,10,10,26.0
    |720609,00201,09,11,9999.9
    |,94996,01,01,35.0
    |,94996,02,01,35.1
    |,94996,03,01,35.2
    |,94996,04,01,35.3
    |,94996,05,01,35.4
    |,94996,06,01,35.5
    |,94996,07,01,35.6
    |,94996,08,01,35.7
    |,94996,09,01,35.8
    |,94996,10,01,35.9
    |blahblah,blahblah,08,01,35.7
    |blahblah,blahblah,09,01,35.8
    |blahblah,blahblah,10,01,35.4
    |""".stripMargin

  private val csvStations =
    """
    |007005,,,
    |007026,,+00.000,+000.000
    |007034,,,
    |008268,,+32.950,+065.567
    |010010,,+70.933,-008.667
    |720609,00201,+32.917,-080.633
    |,94996,+40.695,-096.854
    |,33333,,-096.854
    |,96404,,
    |,96406,+64.502,-154.130
    |""".stripMargin

  // Implement tests for the methods of the `Extraction` object

  @Test def `'readStationLines' should only return correct lines from testfile`: Unit = {

    val sl = Extraction.parseStationLines(Q(csvStations))
    val expected = 5
    assertEquals(s"number of valid station lines should equal to $expected", expected, sl.size)
  }

  @Test def `'getStationsMap' should contain correct keys`: Unit = {

    val sl = Extraction.parseStationLines(Q(csvStations))
    val sm = Extraction.getStationsMap(sl)

    val keysAreValid: Boolean =
      sm.keySet.contains(("008268","")) &&
      sm.keySet.contains(("010010","")) &&
      sm.keySet.contains(("720609","00201")) &&
      sm.keySet.contains(("","94996")) &&
      sm.keySet.contains(("","96406"))

    assertEquals("map entries have expected keys:", true, keysAreValid)
  }

  @Test def `'readTemperatureLines' should return correct lines`: Unit = {

    val tl = Extraction.parseTemperatureLines(Q(csv2021))
    val expected = 31
    assertEquals(s"number of valid temperature lines should equal to $expected", expected, tl.size)
  }

  @Test def `'getTemperatureReadings' should return correct no of readings`: Unit = {

    val tl = Extraction.parseTemperatureLines(Q(csv2021))
    val tr = Extraction.getTemperatureReadings(tl,2021)
    val expected = 31
    assertEquals(s"number of valid temperature readings should equal to $expected", expected, tr.size)
  }

  @Test def `'getTemperatureReadings' should convert Fahrenheit to Celsius`: Unit = {

    val tl = Extraction.parseTemperatureLines(Q(csv2021))
    val tr = Extraction.getTemperatureReadings(tl,2021)
    val goodEnough = ~=(tr(0).temperature,2.16,0.01)
    assert(goodEnough, s"$goodEnough is not good enough!")
  }

  @Test def `'locateTemperaturesFromLists' should return correct no of records`: Unit = {

    val lt = Extraction.locateTemperaturesFromLists(2021,Q(csvStations), Q(csv2021) )
    val expected = 28
    assertEquals(s"number of located temperature readings should equal to $expected", expected, lt.size)
  }

  @Test def `'locationYearlyAverageRecords' should return correct no of records`: Unit = {

    val lt = Extraction.locateTemperaturesFromLists(2021, Q(csvStations), Q(csv2021) )
    val lyar = Extraction.locationYearlyAverageRecords(lt)
    val expected = 3
    assertEquals(s"number of located temperature readings should equal to $expected", expected, lyar.size)
  }

  @Test def `'locationYearlyAverageRecords' should return correct averages`: Unit = {

    val lt = Extraction.locateTemperaturesFromLists(2021, Q(csvStations), Q(csv2021) )
    val lya = Extraction.locationYearlyAverageRecords(lt)
    val expected: Boolean = {
      // In Fahrenheit:
      //(70.933,-8.667),34.775
      //(+32.917,-80.633),25.5
      //(+40.695,-96.854),35.45
      lya.toSeq.contains((Location(70.933,-8.667),1.5416666666666665)) &&
      lya.toSeq.contains((Location(+32.917,-80.633),-3.6111111111111107)) &&
      lya.toSeq.contains((Location(+40.695,-96.854),1.9166666666666665))
    }
    //lyar.foreach(println)
    assertEquals("Correct averages: ", true, expected)
  }

  // More or less exactly correct...
  def ~=(x: Double, y: Double, precision: Double) = {
    if ((x - y).abs < precision) true else false
  }

}

// https://alvinalexander.com/scala/scala-class-object-function-convert-multiline-string-to-list-seq/
object Q {

  def apply(s: String): Seq[String] =
    s.split("\n")
      .toSeq
      .map(_.trim)
      .filter(_ != "")

}