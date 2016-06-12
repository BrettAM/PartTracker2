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

	def jobsPage = {
		Source.fromFile("./public/count.html").getLines.mkString
	}

	case class Job(val id: String, val target: Int) {
		var count: Int = 0
	}
	val gson = new Gson();
	val jobs = scala.collection.mutable.HashMap.empty[String,Job]

	class JobSession(val js: JettySession) {
		var jobid: Option[String] = None
		def onMessage(message: String): Unit = {
			if(jobid == None){
				jobid = Some(message)
				println("jobs request id "+message)
				if(!jobs.contains(message)){
					jobs(message) = Job(message,0)
				}
			} else {
				val dir = message
				if(dir == "up") jobs(jobid.get).count += 1
				else if(dir == "down") jobs(jobid.get).count -= 1
			}
			sendMessage(jobs(jobid.get).count.toString())
		}
		def onClose(code: Int, reason: String): Unit = {
			println(jobid+" closed "+reason)
		}
		def sendMessage(message: String) = {
			js.getRemote.sendStringByFuture(message);
		}
	}

	@WebSocket object ClickResponder{
		val connected = scala.collection.mutable.HashMap.empty[JettySession,JobSession]
	    @OnWebSocketConnect
	    def onConnect(remote: JettySession): Unit = {
	    	connected +=( (remote,new JobSession(remote)) )
	    }
	    @OnWebSocketClose
	    def onClose(remote: JettySession, code: Int, reason:String): Unit = {
	    	val js = connected(remote)
	    	js.onClose(code, reason)
	    	connected -= remote
	    }
	    @OnWebSocketMessage
	    def onMessage(remote: JettySession, message: String): Unit = {
	    	connected(remote).onMessage(message)
	    }
	}

	def main(args: Array[String]) = {
		staticFiles.externalLocation("./public")
		webSocket("/socket", ClickResponder.getClass);
		Spark.port(4567)

		get("/joblist"){ (req, res) =>
			val rtn: String = gson.toJson(asJavaIterable(jobs.values))
			System.out.println(rtn)
			rtn
		}

		get("/jobs"){ (req,res) =>
/*			val id: String = req.queryParams("id")
			println("jobs request id "+id)
			if(!jobs.contains(id)){
				jobs(id) = Job(id,0)
			}*/
			jobsPage
		}

		System.console().readLine()
		Spark.stop()
	}
}
