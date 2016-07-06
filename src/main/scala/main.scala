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
/*
  def loadJob(id: String): Option[Job] = {
    if(jobs.contains(id)) Some(jobs(id))
    else None
  }

  class JobSession(val job: Job, val js: JettySession) {
    val listener = job.register(sendMessage(_))
    println("Session for "+job.MO+" accepted")
    def onMessage(message: String): Unit = job.modify(message)
    def onClose(code: Int, reason: String): Unit = {
      job.deregister(listener)
      println("session for "+job.MO+" closed")
    }
    def sendMessage(message: String) = {
      js.getRemote.sendStringByFuture(message);
    }
  }

  @WebSocket object ClickResponder{
    val connected = scala.collection.mutable.HashMap.empty[JettySession,JobSession]
    @OnWebSocketConnect
    def onConnect(remote: JettySession): Unit = {
      val parameters = remote.getUpgradeRequest.getParameterMap
      val job = Option(parameters.get("id"))
                  .flatMap(l => Option(l.get(0)))
                  .flatMap(j => loadJob(j))
      job match {
        case Some(job) => connected += ((remote, new JobSession(job, remote)))
        case None => remote.close(1008, "job connections require a valid id")
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
  }*/

  def main(args: Array[String]) = {
    Spark.staticFiles.externalLocation("./public")
    Spark.port(4567)

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



    /* websocket job
     * connect requires query "id", being a valid job id
     * While open, any changes to that id's count will be transmitted back to the client
     * The client can send "up" or "down" to alter the target job's count
     */
    //Spark.webSocket("/job", ClickResponder.getClass);
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
    Spark.stop()
  }
}
