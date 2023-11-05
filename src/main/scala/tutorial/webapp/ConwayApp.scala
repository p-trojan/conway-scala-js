package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.timers._
import scala.scalajs.js.annotation.JSExportTopLevel
import tutorial.webapp.ConwayApp.checkLivelyhood
import scala.collection.immutable.ListMap
import scala.scalajs.js.timers.SetIntervalHandle


object ConwayApp {
  var gridVector: Vector[Vector[Cell]] = Vector.empty[Vector[Cell]]

  def main(args: Array[String]): Unit = {
    val rows, columns = 40

    document.addEventListener("DOMContentLoaded", { (event: dom.Event) => 
      gridVector = setupVector(rows, columns)
      setupWrapper(gridVector)
      addNextStepButton()
      addGenerateCellsButton()
      addRunButton()
    })
  }

  @JSExportTopLevel("nextStep")
  def nextStep(): Unit = {
    val cellMap = neigborCounterMap(gridVector)
    val livelyhoodMap = checkLivelyhood(cellMap)
    livelyhoodMap.map((cell, livelyhood) => (cell.setLivelyhood(livelyhood)))
  }

  def neigborCounterMap(inputGrid: Vector[Vector[Cell]]): Map[Cell, Int] = {
    val weight = inputGrid.flatten.map(el => countAliveNeighbours(el))
    val cell = inputGrid.flatten
    val cellCounterMap: Map[Cell, Int] = (cell zip weight).toMap
    ListMap(cellCounterMap.toSeq.sortBy(_._1.id):_*)
  }

  def countAliveNeighbours(cell: Cell): Int = {
    var counter: Int = 0
    for 
      i <- -1 to 1
      j <- -1 to 1
      if (i, j) != (0, 0)
    do
      if gridVector.applyC(cell.row + i).applyC(cell.column + j).livelyhood == Livelyhood.Alive then counter = counter + 1
    counter
  }

  def checkLivelyhood(inputMap: Map[Cell, Int]): Map[Cell, Livelyhood] = {
    def cellNextLivelyhood(cell: Cell, neighbors: Int) = neighbors match
      case n if n < 2 & cell.livelyhood == Livelyhood.Alive => Livelyhood.Dead
      case n if (n == 2 | n == 3) & cell.livelyhood == Livelyhood.Alive => Livelyhood.Alive
      case n if n > 3 & cell.livelyhood == Livelyhood.Alive => Livelyhood.Dead
      case n if n == 3 & cell.livelyhood == Livelyhood.Dead => Livelyhood.Alive
      case n if n != 3 & cell.livelyhood == Livelyhood.Dead => Livelyhood.Dead
    
    inputMap.map((cell, neighbors) => (cell, cellNextLivelyhood(cell, neighbors)))
  }

  def appendParagraph(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }

  def generateCells() = {
    val random = scala.util.Random
    val tenth = gridVector.length * gridVector(0).length * 0.1
    for 
      n <- 1 to tenth.toInt
    do 
      val i = random.nextInt(gridVector.length)
      val j = random.nextInt(gridVector.length)
      gridVector(i)(j).setLivelyhood(Livelyhood.Alive)
  }

  def addRunButton(): Unit = {
    val buttonNode = document.createElement("button")
    buttonNode.textContent = "Run!"
    buttonNode.id = "run-btn"
    buttonNode.addEventListener("click", { (event: dom.MouseEvent) =>
        setInterval(125){ nextStep() }
    })
    document.body.appendChild(buttonNode)
  }

  def addGenerateCellsButton(): Unit = {
    val buttonNode = document.createElement("button")
    buttonNode.textContent = "Generate cells"
    buttonNode.id = "generate-btn"
    buttonNode.addEventListener("click", { (event: dom.MouseEvent) =>
      generateCells()
    })
    document.body.appendChild(buttonNode)
  }

  def addNextStepButton(): Unit = {
    val buttonNode = document.createElement("button")
    buttonNode.textContent = "Next step!"
    buttonNode.id = "next-step-btn"
    buttonNode.addEventListener("click", { (event: dom.MouseEvent) =>
      nextStep()
    })
    document.body.appendChild(buttonNode)
  }

  def appendWrapper(targetNode: dom.Node, rows: Int, columns: Int): dom.Node = {
    val wrapperNode = document.createElement("div")
    wrapperNode.setAttribute("class", "wrapper")
    wrapperNode.setAttribute("style", s"""
                                        display: grid;
                                        grid-template-columns: repeat($columns, 1fr);
                                        grid-template-rows: repeat($rows, 1fr);
                                        width: 700px;
                                        height: 700px;
                                        gap: 1px;
                                        """)
    targetNode.appendChild(wrapperNode)
  }

  def setupWrapper(vector: Vector[Vector[Cell]]) = {
    val wrapper = appendWrapper(document.body, vector.length, vector(0).length)
    vector.flatten.foreach(item => wrapper.appendChild(item.cellDiv))
  }

  def setupVector(rows: Int, columns: Int): Vector[Vector[Cell]] = {
    Vector.tabulate(rows, columns) { (i, j) => Cell(s"${i}_${j}", i, j, Livelyhood.Dead) }
  }

  extension[T](circular: Vector[T])
    def applyC(index: Int): T =
      val size = circular.size
      val modulo = Math.abs(index) % size
      val indexC =
        if index < 0 && modulo != 0 then size - modulo
        else modulo
      circular(indexC)
  
}

case class Cell(id: String, val row: Int, val column: Int, var livelyhood: Livelyhood):
  import Cells._
  import Livelyhood._

  val cellDiv = document.createElement("div")
  cellDiv.setAttribute("style", "background: grey;")
  cellDiv.id = id
  
  cellDiv.addEventListener("click", { (event: dom.MouseEvent) =>
      swapColour(livelyhood)
  })

  def setLivelyhood(livelyhood: Livelyhood) = {
    livelyhood match
      case Alive => {
        cellDiv.setAttribute("style", "background: gold;")
        this.livelyhood = Alive
      }
      case Dead => {
        cellDiv.setAttribute("style", "background: grey;")
        this.livelyhood = Dead
      }
  }

  def swapColour(livelyhood: Livelyhood): Unit = {
    cellDiv.getAttribute("style") match
      case """background: gold;""" => { 
        cellDiv.setAttribute("style", "background: grey;")
        this.livelyhood = Livelyhood.Dead
      }
      case """background: grey;""" => { 
        cellDiv.setAttribute("style", "background: gold;")
        this.livelyhood = Livelyhood.Alive
      }
  }

object Cells:
  opaque type Cell = dom.Node
  
  object Cell:
    def apply(node: dom.Node): Cell = node

enum Livelyhood:
    case Alive
    case Dead
