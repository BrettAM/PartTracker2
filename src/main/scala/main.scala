package com

import com.Serialization._

import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Try

import java.io.FileReader
import java.io.FileWriter
import java.io.File

import com.google.gson.Gson
import com.SparkLib._
import org.eclipse.jetty.websocket.api.annotations._;
import org.eclipse.jetty.websocket.api.{Session => JettySession};
import spark.Spark

//TODO
//periodic backup at user specified interval
//test backup system
//web page themeing
//phat jar packed configuration for static files

object Main {
  val usageMessage: String =
    """Altek Part Tracker Webserver
      |Usage: PartTrackerServer FILE [PORT] [BACKUP_MINUTES]
      |FILE = the file to read and write the log of the MO database
      |PORT = the TCP port to serve app pages on, default 80
      |BACKUP_MINUTES = the number of minutes in between automatic backups
      |        of the MO database, default 30
      |The MO database is saved on shutdown automatically
      |Author: Brett Menzies (scimor5@gmail.com)
    """.stripMargin

  type MODB = scala.collection.mutable.Map[String,MO]
  /**
   * Since normal classOf[] objects don't carry enough information about
   * parameterized subtypes to support recursive deserialization with json,
   * This proxy object's class field listing will have to be used to
   * get that data at runtime
   *
   * I'm not totally convinced this is the only way
   */
  case class SerializationProxy(val data: MODB)

  /** Map containing the database of MOs by MO id string */
  var MOs: MODB = scala.collection.mutable.HashMap.empty[String,MO]

  /**
   * Parse a double but disallow NaN and Infinity like values
   * @type {Object}
   */
  def safeParseDouble(s: String): Double = {
    val d = s.toDouble
    if(d.isInfinite || d.isNaN){
      throw new RuntimeException("Illegal (nan) double value as input")
    }
    d
  }

  /**
   * Class representing an active work socket connection
   * @param mo  Mo being worked on
   * @param op  Operation being worked on
   * @param job WorkSession being worked on
   * @param js  websocket data is moving through
   */
  class JobSession(
    val mo: MO,
    val op: Operation,
    val job: WorkSession,
    val js: JettySession
  ) {
    val expectedPPH = (op.PPH * job.crewSize.toDouble)

    //First message should be the expected parts per hour
    sendMessage( expectedPPH.toString )

    /** POD class for JSON translation for data updates from the work page */
    class WorkPageMessage(val count: Long, val elapsed: Long)

    /** POD class for JSON translation containing a summary of this session */
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

    /**
     * Handler method called by ClickResponder when a message from the
     * work page is received
     */
    def onMessage(message: String): Unit = {
      val update: WorkPageMessage = fromJson[WorkPageMessage](message)
      if(update == null) return

      if(update.count < job.count || update.elapsed < job.elapsed){
        System.err.println("Invalid update from job page")
      } else {
        job.count = update.count
        job.elapsed = update.elapsed
      }
    }

    /** Handler method called by ClickResponder when a session closes */
    def onClose(code: Int, reason: String): Unit = {}

    /** Transmit a message to the work page */
    def sendMessage(message: String) = js.getRemote.sendStringByFuture(message);

    /** Return a summary of this session */
    def summarize = new JobSessionSummary
  }

  @WebSocket object ClickResponder{
    val connected = scala.collection.mutable.HashMap.empty[JettySession,JobSession]
    @OnWebSocketConnect
    def onConnect(remote: JettySession): Unit = {
      //Get the socket data
      val parameters = remote.getUpgradeRequest.getParameterMap
      val mo: Try[MO] = Try( MOs( parameters.get("MO").get(0) ) )
      val opnum: Try[Int] = Try( parameters.get("opnum").get(0).trim.toInt )
      val employee: Try[String] = Try( parameters.get("empl").get(0) )
      val crewSize: Try[Int] = Try{
        val e = parameters.get("crew").get(0).trim.toInt
        if(e <= 0) throw new RuntimeException("No negative crew sizes")
        e
      }

      //try and construct a worksession
      val ws = for(m <- mo; o <- opnum; e <- employee; c <- crewSize) yield {
        m.getOperation(o) match {
          case Some(op) => (m,op,op.newSession(e,c))
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

    /** Return a seq of currently running job sessions */
    def activeSessions: Seq[JobSession] = connected.values.toSeq
  }

  def main(args: Array[String]): Unit = {
    if(args.length < 1){
      println(usageMessage)
      return
    }

    val dbFileName = args(0)
    val port = if(args.length >= 2) args(1).toInt else 80
    val updateInterval = if(args.length >= 3) args(2).toInt else 30

    println(s"Saving to $dbFileName every $updateInterval minutes")

    val dbFile = new File(dbFileName)
    if(!dbFile.exists()) dbFile.createNewFile

    if(!dbFile.canWrite()){
      println("Error: Cannot write to database output file "+dbFileName)
      exit
    }

    loadDatabase(dbFile)

    // Startup the webserver
    setupServer(port)

    // Register a shutdown hook to write the MO database
    Runtime.getRuntime().addShutdownHook(new Thread{
      override def run(): Unit = writeDatabase(dbFile)
    })

    // write MO DB at regular interval, preferrably specified somewhere external
    import java.util.concurrent._
    val executor = new ScheduledThreadPoolExecutor(1)
    executor.scheduleAtFixedRate(
      new Runnable { def run() = writeDatabase(dbFile) },
      0 /*initialDelay*/,
      updateInterval /*Period*/,
      TimeUnit.MINUTES)

    //shutdown after receiving any input just for debugging speed
    System.console().readLine()
    executor.shutdownNow()
    Spark.stop()
    writeDatabase(dbFile)
  }

  def writeDatabase(file: File): Unit = {
    file.synchronized {
      val writer = new FileWriter(file, false /*overwrite*/)
      toJson(SerializationProxy(MOs), writer)
      writer.close
    }
  }

  def loadDatabase(file: File): Unit = {
    val fr = new FileReader(file)
    val inputDB = fromJson[SerializationProxy](fr)
    fr.close()

    MOs = if (inputDB!=null && inputDB.data!=null) inputDB.data
          else scala.collection.mutable.HashMap.empty[String,MO]
  }

  def setupServer(port: Int) {
    println(s"Starting webserver on port $port")
    //Initialize webserver
    Spark.staticFiles.externalLocation("./public")
    Spark.port(port)

    /**
     * create websocket for part counting
     * Requests should include MO, opnum, empl (employeee id),
     *   and crew (crew size) as parameters
     * After connecting, the socket will transmit the expected PPH for the job
     * From then on the page can transmit JSON objects with count and elapsed
     *   field that can be used to update the server to work progress
     */
    Spark.webSocket("/job", ClickResponder.getClass);

    /** Retrieve A list of all active jobs as summaries */
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
      if(!MOs.contains(id) && id != "") {
        val mo = new MO(id)
        MOs(id) = mo
      }
      ""
    }

    /**
     * Register a new Operation
     * Returns an empty string on success, or a string containing an error
     *   message on failure
     * MO The MO to register the operation under
     * dept The department the Operation is associated with
     * opnum The operation's index (must be an integer)
     * pph The operations parts per hour (must be a real number)
     */
    post("/operation"){ (req, res) =>
      val moid = req.queryParams("MO")
      val department = req.queryParams("dept")
      val opNum = Try(req.queryParams("opnum").toInt)
      val pph = Try(safeParseDouble(req.queryParams("PPH")))
      val MO = Try(MOs(moid))

      if (opNum.isFailure) "Invalid Operation Number"
      else if (department.isEmpty) "Department required"
      else if (pph.isFailure || pph.get <= 0.0) "Invalid PPH"
      else if (MO.isFailure) "Invalid MO number"
      else {
        val mo = MO.get
        if(mo.getOperation(opNum.get) != None) {
          if(department == "delete"){
            mo.removeOperation(opNum.get)
            ""
          } else {
            "Operation Number Already Exists;" +
            " To remove an existing operation, enter \"delete\""+
            " as the department name"
          }
        } else {
          val op = new Operation(opNum.get, department, pph.get)
          mo.addOperation(op)
          ""
        }
      }
    }

  }
}
