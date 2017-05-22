package w09_maze.src.main.scala.maze

import java.awt.Color


object Technicolor {

  var hue = 0.0f

  def getNextColor: Color = {

    hue += 0.002f
    if (hue >= 1) hue = 0

  return new Color(Color.HSBtoRGB(hue,1,1))

  }

}