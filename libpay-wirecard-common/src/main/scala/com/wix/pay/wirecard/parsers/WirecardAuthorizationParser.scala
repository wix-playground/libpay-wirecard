package com.wix.pay.wirecard.parsers

import com.wix.pay.wirecard.WirecardAuthorization

trait WirecardAuthorizationParser {
  def parse(authorizationKey: String): WirecardAuthorization
  def stringify(authorization: WirecardAuthorization): String
}
