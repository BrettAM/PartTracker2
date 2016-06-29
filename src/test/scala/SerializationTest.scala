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

  "A Deserialized WorkSession" should "Have Correct Values" in {
    val r = fromJson[WorkSession](
      """{"employee":"bob","crewSize":1,"start":2,"elapsed":3,"count":4}""")
    r.employee should be ("bob")
    r.crewSize should be (1)
    r.start should be (2)
    r.elapsed should be (3)
    r.count should be (4)
  }

  "A Serialized WorkSession" should "Deserialize equally" in {
    val initial = new WorkSession("bob",1)
    initial.count = 10
    initial.elapsed = 20
    val result = fromJson[WorkSession](toJson(initial))
    result.employee should be (initial.employee)
    result.crewSize should be (initial.crewSize)
    result.start should be (initial.start)
    result.elapsed should be (initial.elapsed)
    result.count should be (initial.count)
  }

  "A Deserialized Operation" should "Work" in {
    val initial = new Operation(1,"Paint",10)

    val session = initial.newSession("Session",3)
    session.count = 20

    val result = fromJson[Operation](toJson(initial))

    result.number should be (initial.number)
    result.department should be (initial.department)
    result.PPH should be (initial.PPH)
    result.completed should be (20)
  }

  "A deserialized operation" should "not fail" in {
    val initial = new Operation(1,"Paint",10)

    val session = initial.newSession("Session",3)
    session.count = 20

    val result = fromJson[Operation](toJson(initial))

    result.number should be (initial.number)
    result.department should be (initial.department)
    result.PPH should be (initial.PPH)
    result.completed should be (20)
  }

  "A deserialized MO" should "be keep its operations and counts" in {
    val mo = new MO("A")
    val op = new Operation(1, "Paint", 10)
    val ws = op.newSession("Session",2)
    ws.count = 20
    mo.addOperation(op)
    val json = toJson(mo)
    val result = fromJson[MO](json)
    result.getOperations.foreach(println)
    result.getOperation(1).get.completed should be (20)
  }
}
