package com

case class Job(
  val MO: String,
  val OP: String,
  val Dept: String,
  val Employee: String,
  val PPH: String,
  val Crew: String
) {
  type Listener = String => Unit

  private var count: Int = 0

  // this is lazy so it will still be initialized even if we are constructed by gson
  @transient private lazy val watchList =
    scala.collection.mutable.ListBuffer.empty[Listener]

  def register(upd: Listener): Listener = {
    watchList += upd
    sendCount()
    upd
  }

  def deregister(upd: Listener) = {
    watchList -= upd
  }

  def modify(dir: String) = {
    if(dir == "up") count += 1
    else if(dir == "down") count -= 1
    sendCount()
  }

  private def sendCount() = {
    transmit(count.toString)
  }

  private def transmit(message: String) = {
    watchList.foreach(f => f(message))
  }
}
