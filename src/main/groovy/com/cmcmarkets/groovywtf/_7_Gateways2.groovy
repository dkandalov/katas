package com.cmcmarkets.groovywtf

TFSGateway.listenToTrades(PROD) { trade -> println trade }

def tnPositions = TNGateway.fetchPositions(PROD)
TNGateway.forEachReplayedTrade(PROD, -1) { replayedTrade -> println replayedTrade }

def closePrices = ManifoldGateway.fetchClosePrices(DR)

def internalAccounts = CMGateway.fetchConfigFor(INTERNAL_ACCOUNTS, NFR)

MWGateway.listenToPublishedTrades(NFR) { trade -> println trade }
MWGateway.listenToSequencedTrades(NFR) { trade -> println trade }



