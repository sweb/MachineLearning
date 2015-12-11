package com.github.sweb.machinelearning

import com.github.sweb.machinelearning.utils.DataFrame

/**
  * Created by f.mueller on 08.12.15.
  */
object ExampleDataUtil extends App {

  val data = DataFrame.readFromCsv("src/test/resources/data.csv")

  val train = data.filter(9, "T").toFeatureMatrix

  train.foreach(row => println(row.mkString(",")))



}



