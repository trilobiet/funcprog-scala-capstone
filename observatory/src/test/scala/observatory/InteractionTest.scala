package observatory

import com.sksamuel.scrimage.RGBColor
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  val locationsTemperatures = List(
    (new Location(45.0, -90.0), -20.0)
    , (new Location(45.0, 90.0), -10.0)
    , (new Location(0.0, 0.0), 0.0)
    , (new Location(-45.0, -90.0), 0.0)
    , (new Location(-45.0, 90.0), 20.0)
  )

  val colorMap = List(
    (-20.0, Color(255, 255, 255))
    , (-10.0, Color(255, 255, 0))
    , (0.0, Color(255, 0, 0))
    , (10.0, Color(0, 255, 0))
    , (20.0, Color(0, 0, 255))
  )

  @Test def `'Sampled pixel color must match expected color`: Unit = {

    val img = Interaction.tile(locationsTemperatures, colorMap, Tile(0, 0, 0))
    val sampleColor = img.pixel(127,127).toColor
    val expectedColor = RGBColor(255, 0, 0, 127)
    assertEquals(s"Sampled pixel color $sampleColor must match expected color $expectedColor", expectedColor, sampleColor)
  }
}
