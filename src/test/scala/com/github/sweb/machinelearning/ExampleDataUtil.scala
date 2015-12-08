package com.github.sweb.machinelearning

/**
  * Created by f.mueller on 08.12.15.
  */
object ExampleDataUtil extends App {

  val data = DataFrame.readFromCsv("src/test/resources/data.csv")

  println(data.header.mkString(","))
  println("-" * 80)
  println(data.body.mkString("\n"))

}

case class DataFrame(header: Array[String], body: List[String])

object DataFrame {
  def readFromCsv(filename: String): DataFrame = {
    val source = scala.io.Source.fromFile(filename)
    val lines = try source.getLines.toList finally source.close

    val header = lines.head.split(',').map(_.trim)

    DataFrame(header, lines.tail)
  }
}
