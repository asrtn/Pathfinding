package w09_maze.src.main.scala.maze

import cslib.window.SimpleWindow
import java.awt.Color
import java.io.File
import java.nio.file.Paths

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io._



object AMazeIngRace {
  def printMazesFromDir(path: String): Unit = {
    for (mazeNbr <- 1 to 4) {
      val filename = s"maze$mazeNbr.txt"
      val maze = Maze.fromFile(path + filename)
      println(maze)
    }
  }

  def drawMazesInDir(path: String, window: SimpleWindow): Unit = {
    for (mazeNbr <- 1 to 4) {
      val filename = s"maze$mazeNbr.txt"
      window.moveTo(10, 100)
      window.writeText(s"Click to draw $path$filename")
      window.waitForMouseClick()
      window.clear()




      //Run this code when draw in Maze and walk in MazeTurtle are implemented
      val maze = Maze.fromFile(path + filename)
      maze.draw(window)
      val turtle = new MazeTurtle(window, maze, Color.MAGENTA)
      window.moveTo(10, 50)
      window.writeText(s"Click to walk!")
      window.waitForMouseClick()
      //turtle.walk()
      window.waitForMouseClick()


    }
  }


  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.FileWriter(f, true))
    try { op(p) } finally { p.close() }
  }

  def output(file: File, result: (Long,Int,Int), n: Int, j: Int): Unit ={
    val data = Array(n, j, result._1,result._2, result._3)

    printToFile(file) { p =>
      p.println(data.mkString(" , "))
    }
  }


  def createAndDrawRandomMaze(rows: Int, cols: Int, window: SimpleWindow): Unit = {
    if (rows < 20 || cols < 20) {
      println("Please choose a larger value for rows and cols!")
    } else {
      window.clear()
      window.moveTo(0, 0)


      //Run this code when random in Maze is implemented
      val maze = Maze.random(rows, cols)
      maze.draw(window)
      val turtle = new MazeTurtle(window, maze, Color.RED)
      window.moveTo(10, 70)

      val result = turtle.walk(true)


    }
  }
  def runTest(filePath: String, size: (Int, Int), n: Int, window: SimpleWindow): Unit ={

    val file1 = new File(filePath + " A* " + size._1 + 'x' + size._2)
    val file2 = new File(filePath + " Djikstra " + size._1 + 'x' + size._2)



    val turtles = new ListBuffer[MazeTurtle]

    for (i <- 0 until n) {
      val maze = Maze.random(size._1, size._2)

      println("Built maze no " + i + ", size " + size._1 + 'x' + size._2)

      turtles.append(new MazeTurtle(window, maze, Color.RED))
    }

    val timer = System.currentTimeMillis()

    var i = 0

    turtles.foreach(x => {

      for (j <- 0 until 10) {
        val result = x.walk(true)
        output(file1, result, i, j)

        println("Completed test no " + i + ", " + j + " for A*, size " + size._1 + 'x' + size._2)
      }

      i += 1
    })

    i = 0

    turtles.foreach(x => {
      for (j <- 0 until 10) {
        val result = x.walk(false)
        output(file2, result, i, j)

        println("Completed test no " + i + ", " + j + " for Djikstra, size " + size._1 + 'x' + size._2)
      }

      i += 1
    })

    println("Completed " + n + " tests for size " + size._1 + 'x' + size._2 + " in " +  (System.currentTimeMillis() - timer) + "ms")

  }

  //def getResourcePath : String = getClass.getResource("/workspace/w09_maze/src/main/resources").getPath  // should work on both win/linux Idea/Eclipse??

  def main(args: Array[String]): Unit = {




    val w = new SimpleWindow(1000, 1000, "Pathfinding")

    w.moveTo(0, 0)
    w.clear()
    createAndDrawRandomMaze(200, 200, w)
    w.moveTo(10, 20)

    //runTest("/h/d3/y/ar8058st-s/EDAA35/Pathfinding Lab/", (10,10), 500, w)
    //w.clear()
    //runTest("/h/d3/y/ar8058st-s/EDAA35/Pathfinding Lab/", (100,100), 500, w)
    //w.clear()
    //runTest("/h/d3/y/ar8058st-s/EDAA35/Pathfinding Lab/", (1000,1000), 500, w)


  }

}