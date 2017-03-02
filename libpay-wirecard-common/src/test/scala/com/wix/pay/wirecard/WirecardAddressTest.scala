package com.wix.pay.wirecard

import java.util.Locale

import com.wix.pay.creditcard.AddressDetailed
import com.wix.pay.model.{Customer, Name}
import com.wix.pay.wirecard.WirecardAddress.Empty
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class WirecardAddressTest extends SpecWithJUnit {

  "Wirecard address" should {

    "have customer fields filled" in new Ctx {
      Empty.withCustomer(someCustomer) mustEqual wirecardAddressWithCustomer
    }

    "have address fields filled" in new Ctx {
      Empty.withDetailedAddress(someAddress) mustEqual wirecardAddressWithAddress
    }

    "remain empty when no customer and address are specified" in new Ctx {
      Empty.withCustomer(None).withDetailedAddress(None).isEmpty must beTrue
    }
  }

  trait Ctx extends Scope {
    val someStreet = "Some st. 20 ap.259"
    val someCity = "New York"
    val someState = "NY"
    val somePostalCode = "10010"

    val someCustomer = Customer(name = Some(Name("first", "last")), phone = Some("123432"),
      email = Some("email@example.com"), ipAddress = Some("192.168.0.0"), fax = Some("1234123"),
      company = Some("newCompany"))

    val someAddress = AddressDetailed(street = Some(someStreet), city = Some(someCity), state = Some(someState),
      postalCode = Some(somePostalCode), countryCode = Some(Locale.US))

    val wirecardAddressWithCustomer = WirecardAddress(firstName = "first", lastName = "last", email = "email@example.com")

    val wirecardAddressWithAddress = WirecardAddress(address1 = someStreet, city = someCity, state = someState,
      zipCode = somePostalCode, country = "US")
  }
}
