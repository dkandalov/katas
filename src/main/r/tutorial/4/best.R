best = function(state, outcome) {
    data = read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (!state %in% data$State) stop("invalid state")
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")

    if (outcome == "heart attack") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" }
    else if (outcome == "heart failure") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" }
    else if (outcome == "pneumonia") { outcome = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" }

    data = subset(data, data[outcome] != "Not Available")
    data[,outcome] = as.numeric(data[,outcome])
    data = data[data$State == state,]
    data = data[order(-data[outcome], data$Hospital.Name),]

    as.character(data[1,]$Hospital.Name)
}
