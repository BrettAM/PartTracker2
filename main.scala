import spark._
import spark.Spark
import spark.Spark._

import com.google.gson.Gson

import scala.collection.JavaConversions._
import scala.io.Source

import org.eclipse.jetty.websocket.api.{Session => JettySession};
import org.eclipse.jetty.websocket.api.annotations._;

object Main {
  type Route_t = (Request,Response)=>AnyRef
  def get(ext: String)(f: Route_t) = Spark.get(ext, new sRoute(f));
  def post(ext: String)(f: Route_t) = Spark.post(ext, new sRoute(f));
  def put(ext: String)(f: Route_t) = Spark.put(ext, new sRoute(f));
  def delete(ext: String)(f: Route_t) = Spark.delete(ext, new sRoute(f));
  class sRoute(val f: Route_t) extends Route {
    def handle(q: Request, p: Response): AnyRef = f(q,p)
  }

  case class Job(
    val MO: String,
    val OP: String,
    val Dept: String,
    val Employee: String,
    val PPH: String,
    val Crew: String
  ) {
    var count: Int = 0
    // this is lazy so it will still be initialized even if we are constructed
    // by gson
    @transient lazy val watchList = scala.collection.mutable.ListBuffer.empty[JobSession]
    def modify(dir: String) = {
      if(dir == "up") count += 1
      else if(dir == "down") count -= 1
      alert(count.toString)
    }
    private def alert(message: String) = {
      watchList.foreach(_.sendMessage(message))
    }
  }
  val gson = new Gson()
  val jobs = scala.collection.mutable.HashMap.empty[String,Job]

  def loadJob(id: String): Option[Job] = {
    if(jobs.contains(id)) Some(jobs(id))
    else None
  }

  class JobSession(val job: Job, val js: JettySession) {
    job.watchList += this
    sendMessage(job.count.toString)
    println("Session for "+job.MO+" accepted")
    def onMessage(message: String): Unit = job.modify(message)
    def onClose(code: Int, reason: String): Unit = {
      job.watchList -= this
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
    staticFiles.externalLocation("./public")
    Spark.port(4567)

    /* websocket job
     * connect requires query "id", being a valid job id
     * While open, any changes to that id's count will be transmitted back to the client
     * The client can send "up" or "down" to alter the target job's count
     */
    webSocket("/job", ClickResponder.getClass);
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

    System.console().readLine()
    Spark.stop()
  }
}
