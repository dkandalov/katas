package com.cmcmarkets.groovywtf

def closePrices = [1, 2, 3]

Storage.save("myClosePrices", closePrices)
assert closePrices == Storage.load("myClosePrices")

closePrices = Storage.cached("myClosePrices") { ManifoldGateway.fetchLatestPrices(PROD) }
def result = Storage.cached("longComputation") { factorial(Long.MAX_VALUE) }



