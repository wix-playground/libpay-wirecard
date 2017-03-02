package com.wix.pay.wirecard.parsers
import com.wix.pay.wirecard.WirecardAuthorization
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonWirecardAuthorizationParser extends WirecardAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): WirecardAuthorization =
    Serialization.read[WirecardAuthorization](authorizationKey)

  override def stringify(authorization: WirecardAuthorization): String =
    Serialization.write(authorization)
}
