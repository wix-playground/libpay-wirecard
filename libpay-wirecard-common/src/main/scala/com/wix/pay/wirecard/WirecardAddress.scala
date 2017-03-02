package com.wix.pay.wirecard

import com.wix.pay.creditcard.AddressDetailed
import com.wix.pay.model.Customer

case class WirecardAddress(firstName: String = "", lastName: String = "", address1: String = "",
                           address2: String = "", city: String = "", zipCode: String = "", state: String = "",
                           country: String = "", phone: String = "", email: String = "") {

  def withDetailedAddress(maybeAddress: Option[AddressDetailed]): WirecardAddress =
    maybeAddress.map(withDetailedAddress).getOrElse(this)

  def withDetailedAddress(address: AddressDetailed): WirecardAddress = this.copy(
    address1 = address.street.getOrElse(""),
    city = address.city.getOrElse(""),
    zipCode = address.postalCode.getOrElse(""),
    state = address.state.getOrElse(""),
    country = address.countryCode.map(_.getCountry).getOrElse("")
  )

  def withCustomer(maybeCustomer: Option[Customer]): WirecardAddress =
    maybeCustomer.map(withCustomer).getOrElse(this)

  def withCustomer(customer: Customer): WirecardAddress = this.copy(
    firstName = customer.firstName.getOrElse(""),
    lastName = customer.lastName.getOrElse(""),
    email = customer.email.getOrElse("")
  )

  def isEmpty: Boolean = this == WirecardAddress.Empty
}

object WirecardAddress {
  val Empty = WirecardAddress()
}