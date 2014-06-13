corr <- function(directory, threshold = 0) {
    id <- 1:332
    join <- function(...){ paste(..., sep = "") }
    pad_to_3 <- function(x) { formatC(x, width = 3, flag = 0) }
    append_csv <- function(x) { join(x, ".csv") }
    file_names <- sapply(pad_to_3(id), append_csv)
    file_paths <- sapply(file_names, function(x){ join(directory, "/", x) })

    all_data <- lapply(file_paths, read.csv)
    complete_data <- lapply(all_data, function(data){ subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)) })
    complete_data <- subset(complete_data, sapply(complete_data, function(x){ nrow(x) > 0 }))
    id <- sapply(complete_data, function(data){ data$ID[[1]] })
    nobs <- sapply(complete_data, nrow)

    filtered_data <- subset(complete_data, nobs > threshold)
    sapply(filtered_data, function(data) {
        cor(data$sulfate, data$nitrate)
    })
}
