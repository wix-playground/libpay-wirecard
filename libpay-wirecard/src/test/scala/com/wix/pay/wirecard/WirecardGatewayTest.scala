package com.wix.pay.wirecard

import java.util.Locale

import com.wix.pay.creditcard.{AddressDetailed, CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Customer, Name, Payment}
import com.wix.pay.wirecard.http.WirecardHttpClient
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.util.Success

class WirecardGatewayTest extends SpecWithJUnit with Mockito {

  "Wirecard Gateway" should {
    "generate new transaction and send purchase http request on sale" in new Ctx {
      givenTransactionId(someTransactionId)
      wirecardGateway.sale(someStringifiedCredentials, someCreditCard, somePayment, Some(someCustomer), None)

      got {
        one(transactionIdProvider).nextTransactionId
        one(httpClient).purchase(someCredentials, someTransactionId, someCreditCard, somePayment, wirecardAddress)
      }
    }

    "generate new transaction and send preauthorize http request on authorize" in new Ctx {
      givenTransactionId(someTransactionId)
      givenPreauthorizationResponse(someAuthorization)

      wirecardGateway.authorize(someStringifiedCredentials, someCreditCard, somePayment, Some(someCustomer), None) must
        beSuccessfulTry(someStringifiedAuthorization)

      got {
        one(transactionIdProvider).nextTransactionId
        one(httpClient).preauthorize(someCredentials, someTransactionId, someCreditCard, somePayment, wirecardAddress)
      }
    }

    "send capture http request on capture" in new Ctx {
      wirecardGateway.capture(someStringifiedCredentials, someStringifiedAuthorization, 102.5)
      got {
        one(httpClient).capture(someCredentials, someAuthorization, 102.5)
      }
    }

    "send void preauthorization http request on void auth" in new Ctx {
      wirecardGateway.voidAuthorization(someStringifiedCredentials, someStringifiedAuthorization)
      got {
        one(httpClient).voidPreauthorization(someCredentials, someAuthorization)
      }
    }
  }

  trait Ctx extends Scope {
    val transactionIdProvider = mock[TransactionIdProvider]
    val httpClient = mock[WirecardHttpClient]
    val wirecardGateway: WirecardGateway = new WirecardGateway(httpClient, transactionIdProvider)

    val someTransactionId = "someTransactionId"

    val someStringifiedCredentials =
      """
        |{
        |  "username" : "test",
        |  "password" : "testPassword",
        |  "businessCaseSignature" : "test"
        |}
      """.stripMargin
    val someCredentials = WirecardMerchant("test")

    val someFirstName = "John"
    val someLastName = "Doe"
    val someEmail = "email@email.com"
    val someStreet = "Some st. 20 ap.259"
    val someCity = "New York"
    val someState = "NY"
    val somePostalCode = "10010"
    val someAddress = AddressDetailed(street = Some(someStreet), city = Some(someCity), state = Some(someState),
      postalCode = Some(somePostalCode), countryCode = Some(Locale.US))

    val someCreditCard = CreditCard("4200000000000000", YearMonth(2019, 1),
      Some(CreditCardOptionalFields
        .withFields(csc = Some("471"), holderName = Some(s"$someFirstName $someLastName"))
        .withBillingAddressDetailed(Some(someAddress))))
    val someCustomer = Customer(Some(Name(someFirstName, someLastName)), email = Some(someEmail))
    val wirecardAddress = WirecardAddress(someFirstName, someLastName, someStreet, "", someCity, somePostalCode,
      someState, "US", "", "email@email.com")

    val somePayment = Payment(CurrencyAmount("EUR", 100.20))

    val someAuthorization = WirecardAuthorization("1234", someTransactionId)
    val someStringifiedAuthorization = s"""{"guWid":"1234","transactionId":"$someTransactionId"}"""

    def givenTransactionId(transactionId: String) =
      transactionIdProvider.nextTransactionId returns transactionId

    def givenPreauthorizationResponse(auth: WirecardAuthorization) =
      httpClient.preauthorize(any, any, any, any, any) returns Success(auth)
  }

}
