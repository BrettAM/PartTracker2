import spark._
import spark.Spark
import spark.Spark._

import com.google.gson.Gson

import scala.io.Source

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

	val counts = scala.collection.mutable.HashMap.empty[String,Int]

	case class Job(val id: Int, val target: Int) {
		var count: Int = 0
	}
	val gson = new Gson();

	def main(args: Array[String]) = {
		val job = Job(5, 10)
		val json = gson.toJson(job)
		println()
		println(job)
		println(json)
		println(gson.fromJson(json,classOf[Job]))
		println()

		staticFiles.externalLocation("./public")
		Spark.port(4567)

		get("/jobs"){ (req,res) =>
			val id: String = req.queryParams("id")
			println("jobs request id "+id)
			if(!counts.contains(id)){
				counts(id) = 0
			}
			jobsPage
		}

		post("/jobs"){ (req,res) =>
			val dir: String = req.body()
			val id: String = req.queryParams("id")

			if(dir == "up") counts(id) += 1
			else if(dir == "down") counts(id) -= 1

			println(id+" : "+dir+" : "+counts(id))

			counts(id).toString()
		}

		System.console().readLine()
		Spark.stop()
	}
}
