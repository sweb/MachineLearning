package com.github.sweb.machinelearning.utils

import scala.util.Try

/**
  * Created by f.mueller on 09.12.15.
  */
object DataFrame {
  def readFromCsv(filename: String): DataFrame = {
    val source = scala.io.Source.fromFile(filename)
    val lines = try source.getLines.toList finally source.close

    val header = lines.head.split(',').map(_.trim)

    val rows = lines.tail.map(row => row.split(',').map(_.trim).map(x => Try(x.toDouble).getOrElse(x)))

    DataFrame(header, rows)
  }
}
case class DataFrame(header: Array[String], body: List[Array[Any]]) {
  def toFeatureMatrix(columns: List[Int]): Array[Array[Double]] = {
    ???
  }
}
