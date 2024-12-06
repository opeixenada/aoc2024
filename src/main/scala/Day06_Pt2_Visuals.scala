import Util.readFile
import scala.annotation.tailrec
import javax.swing._
import java.awt._

@main def day06_2_visual(): Unit = {
  val input = readFile("resources/day06").map(_.toIndexedSeq).toIndexedSeq

  // Dynamic cell size based on grid dimensions
  val maxGridSize = 150
  val minCellSize = 5
  val maxCellSize = 20
  val padding = 20

  // Calculate appropriate cell size based on grid dimensions
  val cellSize = {
    val gridWidth = input.head.length
    val gridHeight = input.length
    val maxDim = math.max(gridWidth, gridHeight)
    math.min(maxCellSize, math.max(minCellSize, 800 / maxDim))
  }

  object Colors {
    val background = new Color(241, 245, 249)
    val obstacle = new Color(71, 85, 105)
    val current = new Color(59, 130, 246)
    val testObstacle = new Color(239, 68, 68)
    val visited = new Color(147, 197, 253)
    val empty = new Color(255, 255, 255)
    val text = new Color(30, 41, 59)
    val border = new Color(226, 232, 240)
  }

  def getSymbols(s: Char): Iterable[(Int, Int)] = for {
    x <- input.indices
    y <- input.head.indices
    if input(x)(y) == s
  } yield x -> y

  val start: (Int, Int) = getSymbols('^').head
  val obstacles: Set[(Int, Int)] = getSymbols('#').toSet

  class GridPanel extends JPanel {
    var currentPos: (Int, Int) = (0, 0)
    var currentDir: (Int, Int) = (-1, 0)
    var currentObstacle: (Int, Int) = (0, 0)
    var seenPositions: Set[(Int, Int)] = Set.empty
    var successCount = 0
    var attemptCount = 0
    val totalAttempts = getSymbols('.').size

    // Calculate grid dimensions
    private val gridWidth = input.head.length * cellSize + (padding * 2)
    private val gridHeight = input.length * cellSize + (padding * 2) + 30

    // Set minimum size to handle small grids
    private val minWidth = math.max(300, gridWidth)
    private val minHeight = math.max(200, gridHeight)

    override def getPreferredSize: Dimension = new Dimension(gridWidth.toInt, gridHeight.toInt)
    override def getMinimumSize: Dimension = new Dimension(minWidth.toInt, minHeight.toInt)
    override def getMaximumSize: Dimension = new Dimension(
      math.min(1200, maxGridSize * maxCellSize + padding * 2),
      math.min(900, maxGridSize * maxCellSize + padding * 2 + 30)
    )

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

      // Draw background
      g2d.setColor(Colors.background)
      g2d.fillRect(0, 0, getWidth, getHeight)

      // Draw progress info
      g2d.setColor(Colors.text)
      g2d.setFont(new Font("SansSerif", Font.BOLD, 12))
      val progressText = s"Testing: $attemptCount/$totalAttempts (Found: $successCount)"
      val metrics = g2d.getFontMetrics
      val textX = (getWidth - metrics.stringWidth(progressText)) / 2
      g2d.drawString(progressText, textX, padding - 5)

      // Center the grid if window is larger than minimum size
      val gridX = padding + (getWidth - (input.head.length * cellSize) - padding * 2) / 2
      val gridY = padding + 5

      // Draw grid border
      g2d.setColor(Colors.border)
      val borderWidth = 1
      g2d.setStroke(new BasicStroke(borderWidth))
      g2d.drawRect(
        gridX - borderWidth,
        gridY - borderWidth,
        input.head.length * cellSize + borderWidth * 2,
        input.length * cellSize + borderWidth * 2
      )

      // Draw cells
      for {
        x <- input.indices
        y <- input.head.indices
      } {
        val xPos = gridX + (y * cellSize)
        val yPos = gridY + (x * cellSize)
        val pos = (x, y)

        val cellColor =
          if (pos == currentPos) Colors.current
          else if (pos == currentObstacle) Colors.testObstacle
          else if (obstacles.contains(pos)) Colors.obstacle
          else if (seenPositions.contains(pos)) Colors.visited
          else Colors.empty

        g2d.setColor(cellColor)
        g2d.fillRect(xPos, yPos, cellSize, cellSize)
      }
    }

    def update(pos: (Int, Int), dir: (Int, Int), obstacle: (Int, Int), seen: Set[((Int, Int), (Int, Int))]): Unit = {
      currentPos = pos
      currentDir = dir
      currentObstacle = obstacle
      seenPositions = seen.map(_._1)
      repaint()
      Thread.sleep(1)
    }

    def startNewAttempt(obstacle: (Int, Int)): Unit = {
      attemptCount += 1
      currentObstacle = obstacle
      seenPositions = Set.empty
      repaint()
    }

    def markSuccessful(): Unit = {
      successCount += 1
      repaint()
    }
  }

  val frame = new JFrame("Day 6.2: Guard Gallivant")
  val gridPanel = new GridPanel()

  frame.setLayout(new BorderLayout())
  frame.add(gridPanel, BorderLayout.CENTER)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setResizable(false)
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

  def turnRight(direction: (Int, Int)): (Int, Int) = direction match
    case (-1, 0) => (0, 1)
    case (1, 0)  => (0, -1)
    case (0, 1)  => (1, 0)
    case _       => (-1, 0)

  def oob(xy: (Int, Int)): Boolean = {
    val (x, y) = xy
    x >= input.length || x < 0 || y >= input.head.length || y < 0
  }

  def move(xy: (Int, Int), direction: (Int, Int)): (Int, Int) =
    (xy._1 + direction._1, xy._2 + direction._2)

  @tailrec
  def walk(xy: (Int, Int), direction: (Int, Int) = (-1, 0), seen: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] =
    move(xy, direction) match
      case next if oob(next) => seen + xy
      case next if obstacles.contains(next) => walk(xy, turnRight(direction), seen)
      case next => walk(next, direction, seen + xy)

  @tailrec
  def isLoop(
      xy: (Int, Int),
      direction: (Int, Int) = (-1, 0),
      seen: Set[((Int, Int), (Int, Int))] = Set.empty
  )(obstacle: (Int, Int)): Boolean = {
    gridPanel.update(xy, direction, obstacle, seen)

    move(xy, direction) match
      case next if oob(next)                             => false
      case next if seen.contains(next -> direction)      => true
      case next if (obstacles + obstacle).contains(next) => isLoop(xy, turnRight(direction), seen)(obstacle)
      case next                                          => isLoop(next, direction, seen + (xy -> direction))(obstacle)
  }

  val result = walk(start).tail.count { obstacle =>
    gridPanel.startNewAttempt(obstacle)
    val isSuccessful = isLoop(start)(obstacle)
    if (isSuccessful) gridPanel.markSuccessful()
    isSuccessful
  }
  println(s"Final result: $result")
}
