package com.cmcmarkets.groovywtf

def closePrices = [
        bean([symbol: "AAA", closePrice: "123.456"]),
        bean([symbol: "BBB", closePrice: "789.123"])
]
CsvWriter.write("OverrideClosePrices.csv", closePrices)

assert closePrices == CsvReader.readCsv("OverrideClosePrices.csv")

def aaSymbols = closePrices.findAll { it.symbol.matches("AA.*") }
assert aaSymbols == [bean([symbol: "AAA", closePrice: "123.456"])]




