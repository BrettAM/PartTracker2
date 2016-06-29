package com

import com.google.gson._
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.collection.mutable.{Map => MMap}


object Serialization{
  private val gson = (new GsonBuilder())
    .registerTypeHierarchyAdapter(classOf[Seq[_]], new SeqSerializer)
    .registerTypeHierarchyAdapter(classOf[Map[_,_]], new MapSerializer)
    .registerTypeAdapter(classOf[Buffer[_]], new BufferDeserializer)
    .registerTypeAdapter(classOf[MMap[_,_]], new MapDeserializer)
    .create

  def toJson(input: Any): String = gson.toJson(input)
  def fromJson[T: ClassTag](input: String): T =
    gson.fromJson(input, classTag[T].runtimeClass)

  //Make the exception handling in the deserializers better?

  /** Attempt extract a list of type parameters from a Type t */
  private def getTypeParameters(t: Type): Option[Array[_ <: Type]] = t match {
    case p: ParameterizedType => Some(p.getActualTypeArguments)
    case c: Class[_] => Some(c.getTypeParameters)
    case _ => None
  }

  /** Serialize subclasses of Seq as json arrays */
  private class SeqSerializer extends JsonSerializer[Seq[_]] {
    def serialize(src: Seq[_], t: Type, ctx: JsonSerializationContext) = {
      val array = new JsonArray()
      src.foreach(e => array.add(ctx.serialize(e)))
      array
    }
  }

  /**
   * Serialize subclasses of Map as json objects
   * Note that the keys will be stored by their "toString" version
   */
  private class MapSerializer extends JsonSerializer[Map[_,_]] {
    def serialize(src: Map[_,_], t: Type, ctx: JsonSerializationContext) = {
      val map = new JsonObject()
      src.foreach{ case (k,v) => map.add(k.toString, ctx.serialize(v)) }
      map
    }
  }

  /** deserialize a buffer from a json array */
  private class BufferDeserializer extends JsonDeserializer[Buffer[_]] {
    def deserialize(e: JsonElement, t: Type, ctx: JsonDeserializationContext)= {
      val array = if(e.isJsonArray) e.getAsJsonArray
                  else throw new JsonParseException("Can't deserialize Buffer from nonarray")

      val innerType = getTypeParameters(t) match {
        case Some(types) => types(0)
        case None => throw new Exception("Can't find Buffer value type")
      }

      val builder = scala.collection.mutable.Buffer.newBuilder[Any]
      array.iterator.foreach(x => builder += ctx.deserialize(x, innerType))

      builder.result
    }
  }

  /** deserialize a mutable map from a json object */
  private class MapDeserializer extends JsonDeserializer[MMap[_,_]] {
    def deserialize(e: JsonElement, t: Type, ctx: JsonDeserializationContext) = {
      val obj = if(e.isJsonObject) e.getAsJsonObject
                else throw new JsonParseException("Can't deserialize map from nonobject")

      val innertType = getTypeParameters(t) match {
        case Some(types) => types(1)
        case None => throw new Exception("Can't find map key type")
      }

      val builder = scala.collection.mutable.Map.newBuilder[Any,Any]
      obj.entrySet.foreach{ entry =>
        val k = entry.getKey
        val v: Any = ctx.deserialize(entry.getValue,innertType)
        builder +=( (k,v) )
      }

      builder.result
    }
  }

}
