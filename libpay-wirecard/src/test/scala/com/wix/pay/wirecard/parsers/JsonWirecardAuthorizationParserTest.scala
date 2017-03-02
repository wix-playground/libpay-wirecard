package com.wix.pay.wirecard.parsers

import com.wix.pay.wirecard.WirecardAuthorization
import org.specs2.mutable.SpecWithJUnit

class JsonWirecardAuthorizationParserTest extends SpecWithJUnit {

  val parser = new JsonWirecardAuthorizationParser
  val stringifiedAuth = """{"guWid":"someGuWid","transactionId":"transactionId"}"""
  val auth = WirecardAuthorization("someGuWid", "transactionId")

  "JsonWirecardAuthorizationParser" should {

    "parse wirecard authorization from string" in {
      parser.parse(stringifiedAuth) mustEqual auth
    }

    "stringify wirecard authorization" in {
      parser.stringify(auth) mustEqual stringifiedAuth
    }
  }
}
