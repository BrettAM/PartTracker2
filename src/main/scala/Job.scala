package com

import java.util.Date

// A grouping of Operations
class MO(val id: String){
  private val operations = scala.collection.mutable.HashMap.empty[Int,Operation]

  def addOperation(op: Operation): Unit = {
    //check that that number doesn't already exist
    if(operations.contains(op.number)){
      operations(op.number) = op
    } else {
      throw new Exception("Attempt to add operation number that already exists")
    }
  }

  def getOperation(number: Int): Option[Operation] =
    if(operations.contains(number)) Some(operations(number))
    else None

  def getOperations(): Iterable[Operation] = operations.values
}

// A single task to be worked on by a set of WorkSessions
class Operation(
  val number: Int,
  val department: String,
  val PPH: Double
  ){
  val sessions = scala.collection.mutable.Buffer.empty[WorkSession]
  def completed: Long = sessions.map(_.count).sum
  def totalTime: Long = sessions.map(_.elapsed).sum
  def newSession(employee: String, crew: Int) =  {
    val s = new WorkSession(employee, crew)
    sessions += s
    s
  }
}

// A single instance of working on an Operation
class WorkSession(
  val employee: String,
  val crewSize: Int
  ){
  val start: Long = (new Date()).getTime()
  var elapsed: Long = 0
  var count: Long = 0
  override def toString: String = s"WorkSession by $employee with $count clicks"
}

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
