package com.github.sweb.machinelearning

import com.github.sweb.machinelearning.utils.DataFrame

/**
  * Created by f.mueller on 08.12.15.
  */
object ExampleDataUtil extends App {

  val data = DataFrame.readFromCsv("src/test/resources/data.csv")

  println(data.header.mkString(","))
  println("-" * 80)
  println(data.body.map(x => x.mkString("", ",", "")).mkString("\n"))

  println("-" * 80)
  println("-" * 80)

  data.toFeatureMatrix.foreach(row => println(row.mkString(",")))

}



