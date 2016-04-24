object Ring {

  import shapeless._
  import record._
  import ops.hlist.{ ZipWithKeys }
  import syntax.singleton._
  import ops.record.{ Keys, Values, SelectAll, Merger }

  @annotation.implicitNotFound(msg = "Not all field for ${CC} in record ${R}")
  trait NarrowToCaseClass[R <: HList, CC] extends DepFn1[R] with Serializable { type Out = CC }
  object NarrowToCaseClass {
    def apply[R <: HList, CC](implicit ntcc: NarrowToCaseClass[R, CC]) = ntcc
    
    implicit def ntcc[R <: HList, K <: HList, V <: HList, T <: HList, CC](
      implicit gen: LabelledGeneric.Aux[CC, T],
      keys: Keys.Aux[T, K],
      selectAll: SelectAll.Aux[R, K, V],
      withKeys: ZipWithKeys.Aux[K, V, T]): NarrowToCaseClass[R, CC] = 
      new NarrowToCaseClass[R, CC] {
        def apply(rec: R): CC = {
          val vals = selectAll(rec)
          val newRec = vals.zipWithKeys(keys())
          gen.from(newRec)
        }
      }
  }

  implicit class RecOps[R <: HList](rec: R) {
    def toCaseClass[CC](implicit narrow: NarrowToCaseClass[R, CC]) = narrow(rec)

    def mapFn[A, B, T <: HList](fn: A => B)(
      implicit genB: LabelledGeneric.Aux[B, T],
      narrow: NarrowToCaseClass[R , A],
      merger: Merger[R, T]) = {
      val result = fn(narrow(rec))
      rec.merge(genB.to(result))
    }
  }

  trait Handler[Req <: HList, Rsp <: HList] {
    def apply(req: Req): Rsp
  }

  trait Middleware[Req, NextReq, NextRsp, Rsp] {
    def apply(req: Req, next: Handler[NextReq, NextRsp]): Rsp

    def handle[ReqR <: HList, RspR <: HList, N <: Handler ](req: ReqR, next: H) : RespR = {
      req.mapFn(

    }
  }

  case class ReqWithHeaders(headers: List[(String, String)])
  case class ReqWithCookies(cookies: List[String])
  case class RspWithCookies(cookies: List[String])
  case class RspWithHeaders(headers: List[(String, String)])

  class WithCookies extends Handler[ReqWithHeaders, ReqWithCookies, RspWithCookies, RspWithHeaders] {
    def apply(req: ReqWithHeaders, next: ReqWithCookies => RspWithCookies) : RspWithHeaders = {
      var rsp = next(ReqWithCookies(req.headers.map{_._2}))
      RspWithHeaders(Nil)
    }
  }

  /*case class MyAppReq(path: String, cookies: List[String])
  def myApp(req: MyAppReq) = {
    Response(path + " " + rq.cookies.mkString(","))
  }*/

  case class Request(path: String, headers: List[(String, String)])
  case class Response(body: String)

  def main(args: Array[String]) = {
    val reqGen = LabelledGeneric[Request]
    val req = Request("/test", "a" -> "b" :: Nil)
    val rec = reqGen.to(req)

    /*rec
      .mapFn(parseCookies)
      .mapFn(myApp)

    val respGen = LabelledGeneric[Response]
    val resp = 
    println(resp.body)*/

  }
}
