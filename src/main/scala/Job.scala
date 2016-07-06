package com

import java.util.Date

// A grouping of Operations
class MO(val id: String){
  private val operations = scala.collection.mutable.HashMap.empty[String,Operation]

  def addOperation(op: Operation): Unit = {
    //check that that number doesn't already exist
    if(!operations.contains(op.number.toString)){
      operations(op.number.toString) = op
    } else {
      throw new Exception("Attempt to add operation number that already exists")
    }
  }

  def getOperation(number: Int): Option[Operation] =
    if(operations.contains(number.toString)) Some(operations(number.toString))
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
  def newSession(employee: String, crew: Int) = {
    val s = new WorkSession(employee, crew)
    sessions += s
    s
  }

  class OperationSummary(val number: Int, val department: String,
                         val PPH: Double, val partsCompleted: Long,
                         val truePPH: Double){}
  def summary = new OperationSummary(number, department, PPH, completed, 0.0)
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
