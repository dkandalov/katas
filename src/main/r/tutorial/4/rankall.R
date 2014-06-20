rankall = function(outcome, num = "best") {
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
    if (outcome == "heart attack") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" }
    else if (outcome == "heart failure") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" }
    else if (outcome == "pneumonia") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" }

    data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    rankedDataByState = lapply(split(data, data$State), function(data){ rank(data, outcome) })
    result = lapply(rankedDataByState, function(it) {
        if (num == "best") num = 1
        if (num == "worst") num = nrow(it)
        if (num > nrow(it)) {
            NA
        } else {
            names(it)[names(it) == "Hospital.Name"] = "hospital"
            names(it)[names(it) == "State"] = "state"
            it[num,c("hospital", "state")]
        }
    })

    Reduce(function(acc, value){ rbind(acc, value) }, as.vector(result))
}

rank = function(data, outcome) {
    data = subset(data, data[outcome] != "Not Available")
    data[,outcome] = as.numeric(data[,outcome])
    data = data[order(data[outcome], data$Hospital.Name),]
    data
}
