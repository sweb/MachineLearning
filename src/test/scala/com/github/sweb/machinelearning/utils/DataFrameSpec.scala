package com.github.sweb.machinelearning.utils

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by f.mueller on 09.12.15.
  */
class DataFrameSpec extends FlatSpec with Matchers {

  it should "import data from csv" in {

    val data = DataFrame.readFromCsv("src/test/resources/data.csv")

    data.header should be (Array("lcavol","lweight","age","lbph","svi","lcp","gleason","pgg45","lpsa","train"))
    data.body.head should be (Array(-0.579818495,2.769459,50.0,-1.38629436,0.0,-1.38629436,6.0,0.0,-0.4307829,"T"))
    data.body.size should be (97)
  }

  it should "transform imported data to array of array format as used by ML algorithms" in {
    val result: Array[Array[Any]] = Array(Array(0.5, 1.0), Array(1.0, 0.0))
    val testDf = DataFrame(Array("var1", "var2"), result.toList)

    testDf.toFeatureMatrix should be (result)
  }

  it should "ignore string-columns" in {
    val body = List(Array(0.5, "word", 1.0), Array(1.0, "anotherWord", 0.0))

    val testDf = DataFrame(Array("var1", "var2"), body)

    val result: Array[Array[Any]] = Array(Array(0.5, 1.0), Array(1.0, 0.0))

    testDf.toFeatureMatrix should be (result)

  }

  it should "filter rows" in {
    val body = List(Array(0.5, 1.0, "T"), Array(1.0, 0.0, "F"), Array(-0.5, 0.5, "T"))
    val testDf = DataFrame(Array("var1", "var2", "indic"), body)

    testDf.filter(2, "T").body.map(_.toList) should equal (List(List(0.5, 1.0, "T"), List(-0.5, 0.5, "T")))
  }

  it should "select columns" in {
    val body = List(Array(0.5, 1.0, "T"), Array(1.0, 0.0, "F"), Array(-0.5, 0.5, "T"))
    val testDf = DataFrame(Array("var1", "var2", "indic"), body)

    val actual = testDf.select(List(0, 1))
    actual.header should be (Array("var1", "var2"))
    actual.body.map(_.toList) should be (List(List(0.5, 1.0), List(1.0, 0.0), List(-0.5, 0.5)))
  }

}
