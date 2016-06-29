package com

import org.scalatest._

import com.Serialization._

import scala.collection.mutable.Buffer
import scala.collection.mutable.{Map => MMap}

class ExampleSpec extends FlatSpec with Matchers {
  "A Serialized Map" should "Be A Json Object" in {
    val m = Map(("A" -> true), ("B" -> false))
    toJson(m) should be ("""{"A":true,"B":false}""")
  }

  "A Deserialized Map" should "Be equal to its initial values" in {
    //val m = scala.collection.mutable.Map(("A" -> true), ("B" -> false))
    val m = fromJson[MMap[String,Boolean]]("""{"A":true,"B":false}""")
    m should be (Map(("A" -> true), ("B" -> false)))
  }

  "A Serialized Seq" should "Be a Json Array" in {
    val s = Seq(1,2,3,42)
    toJson(s) should be ("""[1,2,3,42]""")
  }

  "A Deserialized Buffer" should "Be equal to its initial values" in {
    val s = fromJson[Buffer[Int]]("""[1,2,3,42]""")
    s should be (Buffer(1,2,3,42))
  }

  "A Deserialized WorkSession" should "Have Correct Immutable Values" in {
    val initial = new WorkSession("bob",1)
    val result = fromJson[WorkSession](toJson(initial))
    result.employee should be (initial.employee)
    result.crewSize should be (initial.crewSize)
  }

  "A Deserialized WorkSession" should "Have Correct mutable Values" in {
    val initial = new WorkSession("bob",1)
    initial.count = 10
    initial.elapsed = 20
    val result = fromJson[WorkSession](toJson(initial))
    result.count should be (initial.count)
    result.elapsed should be (initial.elapsed)
  }

  "A Deserialized WorkSession" should "Have the right Date" in {
    val initial = new WorkSession("bob",1)
    val result = fromJson[WorkSession](toJson(initial))
    result.start should be (initial.start)
  }

  "A Deserialized Operation" should "Work" in {
    val initial = new Operation(1,"Paint",10)
    initial.newSession("Session",3)
    //System.err.println(toJson(initial))
    val result = fromJson[Operation](toJson(initial))
    //result.sessions.foreach(println)
    //result.number should be (initial.number)
    //result.department should be (initial.department)
    //result.PPH should be (initial.PPH)
  }

}
