import scala.annotation.tailrec
import breeze.linalg._
import breeze.numerics._


object Main extends App {

  def readPopulation(filename: String): List[Int] = {
    val source = io.Source.fromFile(filename)
    val lines = try source.mkString finally source.close()
    lines.split(",").map(_.toInt).toList
  }

  @tailrec
  def runEvolution(epochs: Int, population: List[Int]): List[Int] = (epochs, population) match {
    case (0, population) => population
    case (epochs, population) => runEvolution(epochs - 1, step(population))
  }

  def step(population: List[Int]): List[Int] = population match {
    case Nil => Nil
    case population => population
      .map {case 0 => 6; case x => x - 1}
      .appendedAll(List.fill(population.count(_ == 0))(8))
  }

  def runFastEvolution(epochs: Int, population: List[Int]): Vector[Int] = {

    val x = population.map(i => {
      val vec = Vector.zeros[Int](10)
      vec(i) += 1
      vec(-1) += 1
      vec
    }).reduce(_ + _)
    println(x)

    val A = DenseMatrix(
      (0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
      (1, 0, 0, 0, 0, 0, 0, 1, 0, 0),
      (0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
      (1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      (1, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    )

    pow(A, epochs) * x
  }

  var population = readPopulation("./data/input.txt")
  val epochs = 80
  // val newpopulation = runEvolution(epochs = epochs, population = population)
  val newpopulation = runFastEvolution(epochs = epochs, population = population)
  printf("Amount of fish: %d\n", newpopulation.size)

}
