package w09_maze.src.main.scala.maze

import cslib.window.SimpleWindow
import java.awt.Color

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, Queue}


/**
  * A Turtle class that can be used to walk and draw in a SimpleWindow with a specified color.
  * @param window     The window the turtle should be placed in.
  * @param initPosition   A Point representing the turtle's starting coordinates.
  * @param initAngle      The angle between the turtle direction and the X-axis measured in degrees.
  * @param initPenIsDown  A boolean representing the turtle's pen position. True if the pen is down.
  * @param initColor      The initial color of the pen used when drawing while moving with pen down
  */
class ColorTurtle(window: SimpleWindow,
                  initPosition: Point,
                  initAngle: Double = 90,
                  initPenIsDown: Boolean = true,
                  val initColor: Color)
  extends Turtle(window, initPosition, initAngle, initPenIsDown) {

  /**
    * The color of the pen used when drawing while moving with pen down
    */
  var color: Color = initColor

  /**
    * Moves the turtle forward in its current direction, drawing a line with line color color if the pen is down.
    * @param length The number of pixels to move forward.
    */
  override def forward(length: Double): Unit = {
    window.setLineColor(Technicolor.getNextColor)
    window.setLineWidth(1)
    super.forward(length)
    //window.setLineColor(Color.BLACK)
    //window.setLineWidth(1)
  }

}

class MazeTurtle(window: SimpleWindow, maze: Maze, color: Color) extends ColorTurtle(window, initPosition = Point(maze.getXEntry() + (maze.blockSize / 2) + 1 ,maze.getYEntry() - 1), initColor = color) {


  val initPos =  (maze.data(maze.data.length - 1).indexOf(false), (maze.data.length - 1),0)


  def pathFinding(target: (Int,Int), heuristics: Boolean): (ListBuffer[(Int,Int)], Int) = {

    var currentPos = initPos

    var searched = 0

    def getDistance(a: (Int,Int), b: (Int,Int)): Int ={
      math.abs(a._1 - b._1) + math.abs(a._2 - b._2)
    }

    def score(sq: (Int,Int,Int)): Int = {
      if (heuristics) {
        sq._3 + getDistance((sq._1, sq._2), target)
      } else {
        sq._3
      }
    }

    def diff(e: (Int,Int,Int)) = -score(e)

    val openQueue = PriorityQueue[(Int,Int,Int)]()(Ordering.by(diff))
    openQueue.enqueue(currentPos)


    val closedMatrix = Array.fill(maze.data(1).length, maze.data.length)(-1)


    def getLowestSq(): (Int,Int,Int) = {
      openQueue.dequeue()
    }

    def ColorSquare(x: Int, y: Int, score: Int): Unit = {
      window.setLineWidth(1)
      window.setLineColor(Color.getHSBColor(0,1, (2 * score) / 7))
      val bs = maze.blockSize
      for (j <- 0 until bs) {
          for (i <- 0 until bs) {
            window.moveTo(x * bs + maze.xPadding + j, y * bs + maze.yPadding + i)
            window.lineTo(x * bs + maze.xPadding + j, y * bs + maze.yPadding + i)
          }
        }
    }

    def adjacentSq(sq: (Int,Int,Int)): Vector[(Int,Int,Int)] = {
      val dir = Vector(1,-1)

      var adjacent = Vector[(Int,Int,Int)]()


      for (i <- dir.indices) {
        val x = sq._1 + dir(i)
        val y = sq._2

        try {
          if (!maze.data(y)(x) && closedMatrix(x)(y) == -1) {
            adjacent = adjacent :+ (x,y,sq._3 + 1)

          }
        } catch {
          case x: java.lang.IndexOutOfBoundsException => Unit
        }

      }

      for (i <- dir.indices) {
        val x = sq._1
        val y = sq._2 + dir(i)

        try {

          if (!maze.data(y)(x) && closedMatrix(x)(y) == -1) {
            adjacent = adjacent :+  (x,y,sq._3 + 1)

          }
        } catch {
          case x: java.lang.IndexOutOfBoundsException => Unit
        }


      }

      adjacent
    }





    var foundPath = false


    while (!foundPath && !openQueue.isEmpty){

      currentPos = getLowestSq()

      closedMatrix(currentPos._1)(currentPos._2) = currentPos._3
      searched += 1
      //ColorSquare(currentPos._1, currentPos._2, currentPos._3)

      if (currentPos._1 == target._1 && currentPos._2 == target._2) {
        foundPath = true


        val sequence = ListBuffer[(Int,Int)]()
        sequence.append(target)

        var tempPos = target

        while (tempPos._1 != initPos._1 || tempPos._2 != initPos._2) {

          var leastSq = target

          def isLess(i: Int, j: Int, sq: (Int,Int)): Boolean = {



            if (i >= 0 && i < closedMatrix(0).length && j >= 0 && j < closedMatrix.length) {

              if  (closedMatrix(sq._1)(sq._2) > closedMatrix(i)(j) && closedMatrix(i)(j) != -1) {
                  return true
              }
            }
              return false

          }

          var i = tempPos._1
          var j = tempPos._2 -1

          if (isLess(i,j,tempPos)) {
           leastSq = (i,j)
          }

          i -= 1
          j += 1

          if (isLess(i,j,tempPos)) {
            leastSq = (i,j)
          }

          i += 2

          if (isLess(i,j,tempPos)) {
            leastSq = (i,j)
          }

          i -= 1
          j += 1

          if (isLess(i,j,tempPos)) {
            leastSq = (i,j)
          }

          tempPos = leastSq

          sequence.append(tempPos)

        }

        return (sequence, searched)
      }


      val tempAdjacent = adjacentSq(currentPos)

      for (i <- tempAdjacent.indices) {
        if (closedMatrix(tempAdjacent(i)._1)(tempAdjacent(i)._2) == -1) {

          openQueue.enqueue(tempAdjacent(i))

        }
      }

    }

    return null;

  }


  def setDir(sq: Square): Unit = {

    if (sq.pos.y == sq.parent.pos.y) {

      if(sq.pos.x < sq.parent.pos.x) {
        angle = 180
      } else {
        angle = 0
      }

    } else if (sq.pos.x == sq.parent.pos.x) {

      if(sq.pos.y < sq.parent.pos.y) {
        angle = 90
      } else {
        angle = 270
      }

    }


  }

  def forward(length: Double, n: Int): Unit ={

    for (i <- 0 until n) {
      forward(length / n)
      SimpleWindow.delay(1)
    }
  }

  //Returns time in nanoseconds, pathlength and searched squares
  def walk(heuristics: Boolean): (Long,Int,Int) = {

    val startTime = System.nanoTime()
    val result = pathFinding((maze.exit().pos.x, maze.exit().pos.y), heuristics)
    val time = System.nanoTime() - startTime


    val path = result._1.reverse

    val isq = path.head
    /*
    window.moveTo(isq._1 * maze.blockSize + maze.blockSize / 2 + maze.xPadding, isq._2 * maze.blockSize + maze.blockSize / 2 + maze.yPadding)
    window.setLineWidth(maze.blockSize)
    window.setLineColor(Color.YELLOW)
    path.foreach(sq => {
      window.lineTo(sq._1 * maze.blockSize + maze.blockSize / 2 + maze.xPadding, sq._2 * maze.blockSize + maze.blockSize / 2 + maze.yPadding)
    })
    */

    (time, result._1.length, result._2 )

  }
}