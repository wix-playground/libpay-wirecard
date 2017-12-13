package com.wix.pay.wirecard.http


import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try
import scala.xml.{Elem, XML}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.Post
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.Payment
import com.wix.pay.wirecard.http.WirecardRequestBuilder._
import com.wix.pay.wirecard.{WirecardAddress, WirecardAuthorization, WirecardMerchant}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentRejectedException}


class AkkaWirecardHttpClient(wirecardSettings: WirecardSettings) extends WirecardHttpClient {
  private implicit val system: ActorSystem = ActorSystem("akka-http-wirecard-system")
  private implicit val executionContext: ExecutionContext = system.dispatcher
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  private val http = Http()


  override def purchase(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
                        payment: Payment, address: WirecardAddress): Try[String] = {
    postRequest(credentials, createPurchaseRequest(
      transactionId,
      credentials,
      creditCard,
      payment,
      address)).map(_.guWid)
  }

  override def preauthorize(credentials: WirecardMerchant, transactionId: String, creditCard: CreditCard,
                            payment: Payment, address: WirecardAddress): Try[WirecardAuthorization] = {
    postRequest(credentials, createPreauthorizationRequest(
      transactionId,
      credentials,
      creditCard,
      payment,
      address))
  }

  override def capture(credentials: WirecardMerchant,
                       authorization: WirecardAuthorization,
                       amount: Double): Try[String] =
    postRequest(credentials, createCaptureRequest(authorization, credentials, amount)).map(_.guWid)

  override def voidPreauthorization(credentials: WirecardMerchant,
                                    authorization: WirecardAuthorization): Try[String] =
    postRequest(credentials, createReversalRequest(authorization, credentials)).map(_.guWid)


  private def postRequest(credentials: WirecardMerchant, payload: Elem) = {
    val pipeline: HttpRequest => Future[HttpResponse] = request => http.singleRequest(request)
    val settings = getSettingsForMode(credentials)

    val request = Post(settings.url, payload)
      .withHeaders(Authorization(BasicHttpCredentials(settings.username, settings.password)))

    Try {
      val rawResponse = Await.result(pipeline(request), 30.seconds)
      if (!rawResponse.status.isSuccess) {
        throw PaymentErrorException(s"HTTP error. Status: ${rawResponse.status.intValue}")
      }

      parseWirecardResponse(XML.loadString(Await.result(Unmarshal(rawResponse.entity).to[String], 30.seconds)))
    } recover {
      case e: PaymentException => throw e
      case e                   => throw PaymentErrorException(e.getMessage, e)
    }
  }

  private def getSettingsForMode(credentials: WirecardMerchant): WirecardModeSettings = {
    if (credentials.testMode) wirecardSettings.testSettings else wirecardSettings.liveSettings
  }

  private def parseWirecardResponse(response: Elem) = {
    val result = (response \\ "FunctionResult").text
    val hasError = (response \\ "ERROR").nonEmpty
    if (result == "NOK" || hasError) {
      val error = response \\ "ERROR"
      val message = (error \ "Message").text + " Advice: " + (error \ "Advice").text
      val errorType = (error \ "Type").text
      if (isRejected(errorType, message))
        throw PaymentRejectedException(message)
      else
        throw PaymentErrorException(message)
    }

    val guWid = (response \\ "GuWID").text
    val transactionId = (response \\ "TransactionID").text
    WirecardAuthorization(guWid, transactionId)
  }

  private def isRejected(errorType: String, message: String) =
    errorType == "REJECTED" || message.contains("Credit card number not allowed in demo mode")
}
