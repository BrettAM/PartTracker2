package com

import spark._
import spark.Spark
import spark.Spark._

object SparkLib {
  type Route_t = (Request,Response)=>AnyRef
  class sRoute(val f: Route_t) extends Route {
    def handle(q: Request, p: Response): AnyRef = f(q,p)
  }
  def get(ext: String)(f: Route_t) = Spark.get(ext, new sRoute(f));
  def post(ext: String)(f: Route_t) = Spark.post(ext, new sRoute(f));
  def put(ext: String)(f: Route_t) = Spark.put(ext, new sRoute(f));
  def delete(ext: String)(f: Route_t) = Spark.delete(ext, new sRoute(f));
}
