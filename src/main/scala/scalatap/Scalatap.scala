package scalatap

import rapture.io._

trait Scalatap {
  val base = Http / "api.untappd.com" / "v4"

  protected def resource(path: RelativePath): HttpUrl
}

object Scalatap {

  def apply() = Client

  def apply(clientId: String, clientSecret: String) =
    SecretAuthClient(clientId, clientSecret)

  def redirect(clientId: String, redirectUrl: String) =
    RedirectedOAuthClient(clientId, redirectUrl)


  implicit val encoding = Encodings.`UTF-8`
  import strategy.throwExceptions

  implicit class Dispatcher(val url: HttpUrl) extends AnyVal {
    def json = Json.parse(url.slurp[Char].toString)
  }

  case object Client extends Scalatap {
    protected def resource(path: RelativePath) = base / path.toString()
  }

  case class SecretAuthClient protected[scalatap] (clientId: String, clientSecret: String) extends Scalatap {
    private val auth = Map('client_id -> clientId, 'client_secret -> clientSecret)
    protected def resource(path: RelativePath) = base / path.toString() /? auth

    def userInfo(userName: String) = resource("info" / userName / "user" ).json
    def userCheckins(userName: String) = resource("checkins" / userName / "user" ).json
  }

  sealed trait OAuth {
    val oauthBase = Https / "untappd.com" / "oauth"
  }

  case class RedirectedOAuthClient(clientId: String, redirectUrl: String) extends OAuth {
    lazy val url = (oauthBase / "authenticate" /? Map(
        'client_id -> clientId,
        'response_type -> "code",
        'redirect_url -> redirectUrl
      )).url

    def auth(clientSecret: String, requestToken: String) =
      OAuthClient(clientId, clientSecret, redirectUrl, requestToken)
  }

  object OAuthClient extends OAuth {

    def apply(clientId: String, clientSecret: String, redirectUrl: String, requestToken: String) = {
      val token = (oauthBase / "authorize" /? Map(
        'client_id -> clientId,
        'client_secret -> clientSecret,
        'response_type -> "code",
        'redirect_url -> redirectUrl,
        'code -> requestToken
      )).json.response.access_token.get[String]

      new OAuthClient(token)
    }
  }

  case class OAuthClient protected[scalatap](token: String) {

  }
}
