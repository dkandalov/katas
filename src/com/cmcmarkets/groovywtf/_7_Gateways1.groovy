package com.cmcmarkets.groovywtf

// This is WRONG!!!
// sbc -u sb://nfr-prophet-06:10062/MarketPL deq -v SummaryOut

// This is how it should be
RiskGateway.listenToOutputFrom(MARKETPL, NFR) {
  println it
}
// or like this
def snapshot = RiskGateway.takeSnapshot(EXPOSURE, DR)





