package com

import com.Serialization._

import java.util.Date
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

  class JobSession(val op: Operation, val job: WorkSession, val js: JettySession) {
    println("Session for "+job.employee+" accepted")
    //First message should be the expected parts per hour
    sendMessage( (op.PPH * job.crewSize.toDouble).toInt.toString )
    sendMessage( job.count.toString )

    var timerOn = false
    var startTime = 0L

    def incrementPartCount() = {
      if(timerOn){
        job.count += 1
        sendMessage(job.count.toString)
      }
    }

    def startTimer() = {
      if(!timerOn) {
        timerOn = true
        startTime = (new Date()).getTime()
      }
    }

    def stopTimer() = {
      if(timerOn) {
        timerOn = false
        job.elapsed += ((new Date()).getTime() - startTime)
      }
    }

    def onMessage(message: String): Unit = {
      message match {
        case "1" => incrementPartCount()
        case "start" => startTimer
        case "stop" => stopTimer
        case m => System.err.println("Received bad command "+m)
      }
    }

    def onClose(code: Int, reason: String): Unit = {
      stopTimer()
      println("session for "+job.employee+" closed")
    }

    def sendMessage(message: String) = {
      js.getRemote.sendStringByFuture(message);
    }
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
          case Some(x) => (x,x.newSession(e,c))
          case _ => return;
        }
      }

      //pass it off to a JobSession on success, else close the socket
      if(ws.isFailure) {
        remote.close(1008, "Invalid Parameters")
      } else {
        connected +=( (remote, new JobSession(ws.get._1, ws.get._2, remote)) )
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
  }

  def main(args: Array[String]) = {
    Spark.staticFiles.externalLocation("./public")
    Spark.port(4567)

    /* websocket job
     */
    Spark.webSocket("/job", ClickResponder.getClass);

    MOs("A") = {
      val m = new MO("A")
      m.addOperation(new Operation(1,"Paint",10))
      m.addOperation(new Operation(2,"Fab",20))
      m.addOperation(new Operation(3,"Finish",5))
      m
    }

    get("/mo"){ (req, res) =>
      val id = req.queryParams("MO")
      if(MOs.contains(id)){
        val mo = MOs(id)
        val ops = mo.getOperations.map(_.summary)
        toJson(ops)
      } else {
        ""
      }
    }

    post("/mo"){ (req, res) =>
      val id = req.queryParams("MO")
      if(!MOs.contains(id)) {
        val mo = new MO(id)
        MOs(id) = mo
      }
      ""
    }

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




    /* request a list of all jobs
     * Will return a json object
     */
    /*get("/joblist"){ (req, res) =>
      val rtn: String = gson.toJson(asJavaIterable(jobs.values))
      System.out.println(rtn)
      rtn
    }*/
    /* Request information about a specific job
     * Job is determined by the "id" query paramater
     * Will return a json object if the job exists or an empty string otherwise
     */
    /*get("/jobdata"){ (req, res) =>
      println("Get request")
      val id = req.queryParams("id")
      loadJob(id) match {
        case Some(job) => gson.toJson(job)
        case None => ""
      }
    }*/
    /* Register a new job with the system
     * required data (as json): MO number, Operation number, Department,
     *     parts per hour, crew size, employee number
     * ex. {"MO":"1","OP":"2","Dept":"Paint","PPH":3,"Crew":1,"Employee":"A"}
     * will return true if the job is registered successfully, false otherwise
     */
    /*post("/jobdata"){ (req, res) =>
      println("Post request: "+req.body)
      val job = gson.fromJson(req.body, classOf[Job])
      println(job)
      if(jobs.contains(job.MO)){
        "false"
      } else {
        jobs +=( (job.MO, job) )
        "true"
      }
    }*/


    //shutdown after receiving a line
    System.console().readLine()
    println(toJson(MOs))
    Spark.stop()
  }
}
