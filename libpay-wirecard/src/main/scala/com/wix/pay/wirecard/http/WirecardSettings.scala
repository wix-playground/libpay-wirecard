package com.wix.pay.wirecard.http

case class WirecardSettings(liveSettings: WirecardModeSettings,
                            testSettings: WirecardModeSettings)

case class WirecardModeSettings(url: String, username: String, password: String)
