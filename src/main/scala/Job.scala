package com

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

  def actualPPH: Double = Job.calculatePPH(completed, totalTime)

  class OperationSummary(){
    val number: Int = Operation.this.number
    val department: String = Operation.this.department
    val PPH: Double = Operation.this.PPH
    val partsCompleted: Long = completed
    val truePPH: Double = actualPPH
  }
  def summary = new OperationSummary()
}

// A single instance of working on an Operation
class WorkSession(
  val employee: String,
  val crewSize: Int
  ){
  val start: Long = System.currentTimeMillis()
  var elapsed: Long = 0
  var count: Long = 0
  override def toString: String = s"WorkSession by $employee with $count clicks"
  def actualPPH: Double = Job.calculatePPH(count, elapsed)
}

object Job{
  val millisecondsPerHour = 60.0/*minutes*/ * 60.0/*seconds*/ * 1000.0/*ms*/;
  def calculatePPH(parts: Long, milliseconds: Long) = {
    if(milliseconds == 0) 0.0
    else {
      val hours: Double = milliseconds / millisecondsPerHour
      parts.toDouble / hours
    }
  }
}
