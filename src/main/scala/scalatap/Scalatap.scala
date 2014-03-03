package scalatap

import rapture.io._

trait Scalatap {
  val base = Http / "api.untappd.com" / "v4"
  protected def resource(path: RelativePath): HttpUrl
}

object Scalatap {

  def apply() = UnauthorizedScalatap

  def apply(clientId: String, clientSecret: String) = AuthorizedScalatap(clientId, clientSecret)

  import strategy.throwExceptions
  implicit val encoding = Encodings.`UTF-8`

  implicit class ToJson(val url: HttpUrl) extends AnyVal {
    def json = Json.parse(url.slurp[Char].toString)
  }

  case object UnauthorizedScalatap extends Scalatap {
    protected def resource(path: RelativePath) = base / path.toString()
  }

  case class AuthorizedScalatap(clientId: String, clientSecret: String) extends Scalatap {
    private val auth = Map('client_id -> clientId, 'client_secret -> clientSecret)
    protected def resource(path: RelativePath) = base / path.toString() /? auth

    def userInfo(userName: String) = resource("info" / userName / "user" ).json
    def userCheckins(userName: String) = resource("checkins" / userName / "user" ).json
  }
}
