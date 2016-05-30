import spark._;
import spark.Spark;
import spark.Spark._;

object Main {
	type Route_t = (Request,Response)=>AnyRef
	def get(ext: String)(f: Route_t) = Spark.get(ext, new sRoute(f));
	def post(ext: String)(f: Route_t) = Spark.post(ext, new sRoute(f));
	def put(ext: String)(f: Route_t) = Spark.put(ext, new sRoute(f));
	def delete(ext: String)(f: Route_t) = Spark.delete(ext, new sRoute(f));
	class sRoute(val f: Route_t) extends Route {
		def handle(q: Request, p: Response): AnyRef = f(q,p)
	}

	val counts = scala.collection.mutable.HashMap.empty[String,Int]

	def main(args: Array[String]) = {
		staticFiles.externalLocation("./public");
		post("/start"){ (req,res) =>
			val jid = req.queryParams("jid")
			println("Start request "+jid)
			if(jid != null){
				println("jid not null")
				req.session(true)
				req.session.attribute("jid",jid)
				if(!counts.contains(jid)){
					counts(jid) = 0
				}
				"count.html"
			} else {
				"error.html"
			}
		}
		post("/up"){ (req,res) =>
			//println("up "+req.session.attributes())
			val id: String = req.session.attribute("jid")
			println("Up from "+id)
			if(counts.contains(id)){
				counts(id) += 1
				counts(id).toString()
			} else {
				halt()
				""
			}
		}
		post("/down"){ (req,res) =>
			val id: String = req.session.attribute("jid")
			println("Down from "+id)
			if(counts.contains(id)){
				counts(id) -= 1
				counts(id).toString()
			} else {
				halt()
				""
			}
		}

		System.console().readLine()
		Spark.stop()
	}
}
