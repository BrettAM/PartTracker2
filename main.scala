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

	case class Job(val id: String, val target: Int) {
		var count: Int = 0
		@transient val watchList =
			scala.collection.mutable.ListBuffer.empty[JobSession]
		def modify(dir: String) = {
			if(dir == "up") count += 1
			else if(dir == "down") count -= 1
			alert(count.toString)
		}
		private def alert(message: String) = {
			watchList.foreach(_.sendMessage(message))
		}
	}
	val gson = new Gson();
	val jobs = scala.collection.mutable.HashMap.empty[String,Job]

	class JobSession(val js: JettySession) {
		var job: Option[Job] = None
		def onMessage(message: String): Unit = {
			job match {
				case None => {
					val j = {
						if(jobs.contains(message)) jobs(message)
						else {
							val nj = new Job(message,0)
							jobs(message) = nj
							nj
						}
					}
					job = Some(j)
					j.watchList += this
					sendMessage(j.count.toString)
				}
				case Some(j) => j.modify(message)
			}
		}
		def onClose(code: Int, reason: String): Unit = {
			job match {
				case None => {
					println("Session closed with no job assigned")
				}
				case Some(j) => {
					j.watchList -= this
					println("session for "+j.id+" closed")
				}
			}
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
		//socket doc
		webSocket("/socket", ClickResponder.getClass);
		Spark.port(4567)
		//joblist doc
		get("/joblist"){ (req, res) =>
			val rtn: String = gson.toJson(asJavaIterable(jobs.values))
			System.out.println(rtn)
			rtn
		}
		//jobcreate doc
		//jobdata doc

		System.console().readLine()
		Spark.stop()
	}
}
