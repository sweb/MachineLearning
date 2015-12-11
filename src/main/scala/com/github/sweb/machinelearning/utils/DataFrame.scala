package com.github.sweb.machinelearning.utils

import scala.util.Try

/**
  * Created by f.mueller on 09.12.15.
  */
case class DataFrame(header: Array[String], body: List[Array[Any]]) {

  def toFeatureMatrix(): Array[Array[Double]] = {
    body.map(row => row.filter(_.isInstanceOf[Double]).map(_.asInstanceOf[Double])).toArray
  }

  def filter(column: Int, criterium: String): DataFrame = {
    val filteredBody = body.filter(row => row(column) == criterium)
    DataFrame(header, filteredBody)
  }

  def select(columns: List[Int]): DataFrame = {
    val selectedHeader = columns.map(header(_)).toArray
    val selectedBody = body.map(row => columns.map(row(_)).toArray)
    DataFrame(selectedHeader, selectedBody)
  }
}

object DataFrame {
  def readFromCsv(filename: String): DataFrame = {
    val source = scala.io.Source.fromFile(filename)
    val lines = try source.getLines.toList finally source.close

    val header = lines.head.split(',').map(_.trim)

    val rows = lines.tail.map(row => row.split(',').map(_.trim).map(x => Try(x.toDouble).getOrElse(x)))

    DataFrame(header, rows)
  }
}
