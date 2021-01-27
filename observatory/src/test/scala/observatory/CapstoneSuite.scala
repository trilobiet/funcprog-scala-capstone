package observatory

import org.junit.Assert._
import org.scalacheck.Test

class CapstoneSuite
  extends ExtractionTest
    with VisualizationTest
    with InteractionTest
    with ManipulationTest
    with Visualization2Test
    with Interaction2Test

trait MilestoneSuite {

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

  val csvStations =
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


  def namedMilestoneTest(milestoneName: String, level: Int)(block: => Unit): Unit =
    if (Grading.milestone >= level) {
      block
    } else {
      fail(s"Milestone $level ($milestoneName) is disabled. To enable it, set the 'Grading.milestone' value to '$level'.")
    }

}

