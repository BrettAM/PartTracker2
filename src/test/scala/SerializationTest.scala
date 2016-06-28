package com

import collection.mutable.Stack
import org.scalatest._

import com.google.gson._

class ExampleSpec extends FlatSpec with Matchers {

  import scala.collection.mutable.Buffer
  import java.lang.reflect.Type
  import java.lang.reflect.ParameterizedType
  import scala.collection.JavaConversions._
  import com.google.gson.internal.$Gson$Types

  class SeqSerializer extends JsonSerializer[Seq[_]] {
    def serialize(src: Seq[_], t: Type, ctx: JsonSerializationContext) = {
      val array = new JsonArray()
      src.foreach(e => array.add(ctx.serialize(e)))
      array
    }
  }

  class MapSerializer extends JsonSerializer[Map[_,_]] {
    def serialize(src: Map[_,_], t: Type, ctx: JsonSerializationContext) = {
      val map = new JsonObject()
      src.foreach{ case (k,v) => map.add(k.toString, ctx.serialize(v)) }
      map
    }
  }

  def getTypeParameters(t: Type): Option[Array[_ <: Type]] = {
    t match {
      case p: ParameterizedType => Some(p.getActualTypeArguments)
      case c: Class[_] => Some(c.getTypeParameters)
      case _ => None
    }
  }

  //Make the exception handling in the deserializers better?

  class BufferDeserializer extends JsonDeserializer[Buffer[_]] {
    def deserialize(e: JsonElement, t: Type, ctx: JsonDeserializationContext)= {
      if(e.isJsonArray){
        val innerType = (getTypeParameters(t).get)(0)
        val a = e.getAsJsonArray
        val builder = scala.collection.mutable.Buffer.newBuilder[Any]
        a.iterator.foreach(x => builder += ctx.deserialize(x, innerType))
        builder.result
      } else {
        throw new JsonParseException("Attemt to deserialize Buffer from non array")
      }
    }
  }

  type MMap = scala.collection.mutable.Map[String,_]
  class MapDeserializer extends JsonDeserializer[MMap] {
    def deserialize(e: JsonElement, t: Type, ctx: JsonDeserializationContext)= {
      if(e.isJsonObject){
        val valueType = (getTypeParameters(t).get)(1)
        val a = e.getAsJsonObject
        val builder = scala.collection.mutable.Map.newBuilder[String,Any]
        a.entrySet.foreach{ entry =>
          val k = entry.getKey
          val v: Any = ctx.deserialize(entry.getValue,valueType)
          builder +=( (k,v) )
        }
        builder.result
      } else {
        throw new JsonParseException("Attemt to deserialize Buffer from non array")
      }
    }
  }

  val gson = (new GsonBuilder())
    .registerTypeHierarchyAdapter(classOf[Seq[_]], new SeqSerializer)
    .registerTypeHierarchyAdapter(classOf[Map[_,_]], new MapSerializer)
    .registerTypeAdapter(classOf[Buffer[_]], new BufferDeserializer)
    .registerTypeAdapter(classOf[MMap], new MapDeserializer)
    .create

  "A Serialized Map" should "Be A Json Object" in {
    val m = Map(("A" -> true), ("B" -> false))
    gson.toJson(m) should be ("""{"A":true,"B":false}""")
  }

  "A Deserialized Map" should "Be equal to its initial values" in {
    //val m = scala.collection.mutable.Map(("A" -> true), ("B" -> false))
    val m = gson.fromJson("""{"A":true,"B":false}""", classOf[MMap])
    m should be (Map(("A" -> true), ("B" -> false)))
  }

  "A Serialized Seq" should "Be a Json Array" in {
    val s = Seq(1,2,3,42)
    gson.toJson(s) should be ("""[1,2,3,42]""")
  }

  "A Deserialized Buffer" should "Be equal to its initial values" in {
    val s = gson.fromJson("""[1,2,3,42]""", classOf[Buffer[Int]])
    s should be (Buffer(1,2,3,42))
  }

  "A Deserialized WorkSession" should "Have Correct Immutable Values" in {
    val initial = new WorkSession("bob",1)
    val result = gson.fromJson(gson.toJson(initial), classOf[WorkSession])
    result.employee should be (initial.employee)
    result.crewSize should be (initial.crewSize)
  }

  "A Deserialized WorkSession" should "Have Correct mutable Values" in {
    val initial = new WorkSession("bob",1)
    initial.count = 10
    initial.elapsed = 20
    val result = gson.fromJson(gson.toJson(initial), classOf[WorkSession])
    result.count should be (initial.count)
    result.elapsed should be (initial.elapsed)
  }

  "A Deserialized WorkSession" should "Have the right Date" in {
    val initial = new WorkSession("bob",1)
    val result = gson.fromJson(gson.toJson(initial), classOf[WorkSession])
    result.start should be (initial.start)
  }

  "A Deserialized Operation" should "Work" in {
    val initial = new Operation(1,"Paint",10)
    initial.newSession("Session",3)
    //System.err.println(gson.toJson(initial))
    val result = gson.fromJson(gson.toJson(initial), classOf[Operation])
    //result.sessions.foreach(println)
    //result.number should be (initial.number)
    //result.department should be (initial.department)
    //result.PPH should be (initial.PPH)
  }

}
