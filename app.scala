object App {

  import ring._

  // Cookies Transformations
  case class ReqWithHeaders(headers: List[(String, String)])
  case class ReqWithCookies(cookies: List[String])
  case class RspWithCookies(cookies: List[String])
  case class RspWithHeaders(headers: List[(String, String)])

  def parseCookies(req: ReqWithHeaders): ReqWithCookies = {
    ReqWithCookies(req.headers.map{_._2})
  }

  // App handler
  case class MyAppReq(path: String, cookies: List[String])
  def myApp(req: MyAppReq): Response = {
    Response(req.path + " " + req.cookies.mkString(","))
  }

  // Top level Req/Rsp
  case class Request(path: String, headers: List[(String, String)])
  case class Response(body: String)

  def printResponse(r: Response): Unit = {
    println(r.body)
  }

  def main(args: Array[String]): Unit = {
    // Build a request chain
    val chain = 
      Chain[Request]
        .transform(parseCookies)
        .handle(myApp)
        .transform(printResponse)
        .build[Request]

    // Process a request
    val req = Request("/test", "a" -> "b" :: Nil)
    chain(req)
    // prints "/test b"

  }
}
