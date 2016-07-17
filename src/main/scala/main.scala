package com

import com.Serialization._

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try

import com.google.gson.Gson
import com.SparkLib._
import org.eclipse.jetty.websocket.api.annotations._;
import org.eclipse.jetty.websocket.api.{Session => JettySession};
import spark.Spark

object Main {
  val MOs = scala.collection.mutable.HashMap.empty[String,MO]

  class WorkPageMessage(val count: Long, val elapsed: Long)

  class JobSession(
    val mo: MO,
    val op: Operation,
    val job: WorkSession,
    val js: JettySession
  ) {
    println("Session for "+job.employee+" accepted")

    val expectedPPH = (op.PPH * job.crewSize.toDouble)
    //First message should be the expected parts per hour
    sendMessage( expectedPPH.toString )

    def onMessage(message: String): Unit = {
      System.err.println("Received message "+message)

      val update: WorkPageMessage = fromJson[WorkPageMessage](message)
      if(update == null) return

      if(update.count < job.count || update.elapsed < job.elapsed){
        System.err.println("Invalid update from job page")
      } else {
        job.count = update.count
        job.elapsed = update.elapsed
      }
    }

    def onClose(code: Int, reason: String): Unit = {
      println("session for "+job.employee+" closed")
    }

    def sendMessage(message: String) = js.getRemote.sendStringByFuture(message);

    class JobSessionSummary(){
      val MO: String = mo.id
      val opnum: Int = op.number
      val Employee: String = job.employee
      val crewSize: Int = job.crewSize
      val StartTime: Long = job.start
      val ElapsedTime: Long = job.elapsed
      val count: Long = job.count
      val ExpectedPPH: Double = expectedPPH
      val ActualPPH: Double = job.actualPPH
    }
    def summarize = new JobSessionSummary
  }

  @WebSocket object ClickResponder{
    val connected = scala.collection.mutable.HashMap.empty[JettySession,JobSession]
    @OnWebSocketConnect
    def onConnect(remote: JettySession): Unit = {
      //Get the socket data
      val parameters = remote.getUpgradeRequest.getParameterMap
      val mo: Try[MO] = Try( MOs( parameters.get("MO").get(0) ) )
      val opnum: Try[Int] = Try( parameters.get("opnum").get(0).toInt )
      val employee: Try[String] = Try( parameters.get("empl").get(0) )
      val crewSize: Try[Int] = Try( parameters.get("crew").get(0).toInt )

      //try and construct a worksession
      val ws = for(m <- mo; o <- opnum; e <- employee; c <- crewSize) yield {
        m.getOperation(o) match {
          case Some(x) => (m,x,x.newSession(e,c))
          case _ => return;
        }
      }

      //pass it off to a JobSession on success, else close the socket
      if(ws.isFailure) {
        remote.close(1008, "Invalid Parameters")
      } else {
        val d = ws.get
        connected +=( (remote, new JobSession(d._1, d._2, d._3, remote)) )
      }
    }
    @OnWebSocketClose
    def onClose(remote: JettySession, code: Int, reason:String): Unit = {
      if(connected.contains(remote)){
        val js = connected(remote)
        js.onClose(code, reason)
        connected -= remote
      }
    }
    @OnWebSocketMessage
    def onMessage(remote: JettySession, message: String): Unit = {
      connected(remote).onMessage(message)
    }

    def activeSessions: Seq[JobSession] = connected.values.toSeq
  }

  def main(args: Array[String]) = {
    //debug database entries
    MOs("A") = {
      val m = new MO("A")
      m.addOperation(new Operation(1,"Paint",10))
      m.addOperation(new Operation(2,"Fab",20))
      m.addOperation(new Operation(3,"Finish",5))
      m
    }

    //Initialize webserver
    Spark.staticFiles.externalLocation("./public")
    Spark.port(4567)

    /* websocket job
     */
    Spark.webSocket("/job", ClickResponder.getClass);

    /**
     * Retrieve A list of all active jobs
     *
     */
    get("/active") { (req, res) =>
      toJson(ClickResponder.activeSessions.map(_.summarize))
    }

    /**
     * Retrieve A summary of a particular MO
     * Specify the MO identifier using query parameter "MO"
     * Returns an empty string if the MO does not exist
     */
    get("/mo"){ (req, res) =>
      val id = req.queryParams("MO")
      if(MOs.contains(id)){
        val mo = MOs(id)
        val ops = mo.getOperations.toSeq.sortBy(_.number).map(_.summary)
        toJson(ops)
      } else {
        ""
      }
    }

    /**
     * Register a new MO identifier
     * Specify the MO identifier using query parameter "MO"
     * If the specified MO already exists, no action is taken
     */
    post("/mo"){ (req, res) =>
      val id = req.queryParams("MO")
      if(!MOs.contains(id)) {
        val mo = new MO(id)
        MOs(id) = mo
      }
      ""
    }

    /**
     * Register a new Operation
     * MO The MO to register the operation under
     * dept The department the Operation is associated with
     * opnum The operation's index (must be an integer)
     * pph The operations parts per hour (must be a real number)
     */
    post("/operation"){ (req, res) =>
      val moid = req.queryParams("MO")
      val department = req.queryParams("dept")
      val opNum = Try(req.queryParams("opnum").toInt)
      val pph = Try(req.queryParams("PPH").toDouble)
      val MO = Try(MOs(moid))

      if (opNum.isFailure) "Invalid Operation Number"
      else if (department.isEmpty) "Department required"
      else if (pph.isFailure) "Invalid PPH"
      else if (MO.isFailure) "Invalid MO number"
      else {
        val mo = MO.get
        if(mo.getOperation(opNum.get) != None) "Operation Number Already Exists"
        else {
          val op = new Operation(opNum.get, department, pph.get)
          mo.addOperation(op)
          ""
        }
      }
    }

    //shutdown after receiving any input
    System.console().readLine()
    println(toJson(MOs))
    Spark.stop()
  }
}
