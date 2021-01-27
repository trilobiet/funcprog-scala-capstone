package observatory

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

trait ManipulationTest extends MilestoneSuite {

  private val milestoneTest = namedMilestoneTest("data manipulation", 4) _

  // Implement tests for methods of the `Manipulation` object

  @Test def `'whatever`: Unit = {

    val locationsTemperatures = List(
      (new Location(45, -90), -20.0)
      , (new Location(45, 90), -10.0)
      , (new Location(0, 0), 0.0)
      , (new Location(-45, -90), 0.0)
      , (new Location(-45, 90), 20.0)
    )

    val f: GridLocation => Temperature = Manipulation.makeGrid(locationsTemperatures)
    val actual = f(GridLocation(0,0))
    val expected = 0.0

    assertEquals("blabla",actual,expected,0.00001)
  }


  @Test def `'averages of multiple times the same year must for every grid point be the same value as for that grid point in a single year`: Unit = {

    val lt1 = Extraction.locateTemperaturesFromLists(2020, Q(csvStations), Q(csv2021) )
    val lya1 = Extraction.locationYearlyAverageRecords(lt1)

    val temperaturess: Seq[Iterable[(Location, Temperature)]] = List(lya1,lya1,lya1)

    val avg = Manipulation.average(temperaturess)
    val grid = Manipulation.makeGrid(lya1)

    val trueEverywhere = ! {
      for (
        x <- -89 to 90;
        y <- -180 to 179
      ) yield {
        val loc = GridLocation(x, y)
        math.abs(avg(loc)-grid(loc)) < 0.00001
      }
    }.contains(false)

    assertTrue("averages do not everywhere have the expected value",trueEverywhere)
  }


}
