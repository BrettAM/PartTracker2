package com

import spark.Spark

import com.SparkLib._

import com.google.gson.Gson

import scala.collection.JavaConversions._
import scala.io.Source

import org.eclipse.jetty.websocket.api.{Session => JettySession};
import org.eclipse.jetty.websocket.api.annotations._;

object Main {
  val gson = new Gson()
  val jobs = scala.collection.mutable.HashMap.empty[String,Job]

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
  }

  def main(args: Array[String]) = {
    Spark.staticFiles.externalLocation("./public")
    Spark.port(4567)

    /* websocket job
     * connect requires query "id", being a valid job id
     * While open, any changes to that id's count will be transmitted back to the client
     * The client can send "up" or "down" to alter the target job's count
     */
    Spark.webSocket("/job", ClickResponder.getClass);
    /* request a list of all jobs
     * Will return a json object
     */
    get("/joblist"){ (req, res) =>
      val rtn: String = gson.toJson(asJavaIterable(jobs.values))
      System.out.println(rtn)
      rtn
    }
    /* Request information about a specific job
     * Job is determined by the "id" query paramater
     * Will return a json object if the job exists or an empty string otherwise
     */
    get("/jobdata"){ (req, res) =>
      println("Get request")
      val id = req.queryParams("id")
      loadJob(id) match {
        case Some(job) => gson.toJson(job)
        case None => ""
      }
    }
    /* Register a new job with the system
     * required data (as json): MO number, Operation number, Department,
     *     parts per hour, crew size, employee number
     * ex. {"MO":"1","OP":"2","Dept":"Paint","PPH":3,"Crew":1,"Employee":"A"}
     * will return true if the job is registered successfully, false otherwise
     */
    post("/jobdata"){ (req, res) =>
      println("Post request: "+req.body)
      val job = gson.fromJson(req.body, classOf[Job])
      println(job)
      if(jobs.contains(job.MO)){
        "false"
      } else {
        jobs +=( (job.MO, job) )
        "true"
      }
    }

    //shutdown after receiving a line
    System.console().readLine()
    Spark.stop()
  }
}
