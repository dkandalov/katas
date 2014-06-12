complete <- function(directory, id = 1:332) {
    join <- function(...){ paste(..., sep = "") }
    pad_to_3 <- function(x) { formatC(x, width = 3, flag = 0) }
    append_csv <- function(x) { join(x, ".csv") }
    file_names <- lapply(pad_to_3(id), append_csv)
    file_paths <- lapply(file_names, function(x){ join(directory, "/", x) })

    all_data <- lapply(file_paths, read.csv)
    complete_data <- lapply(all_data, function(data) { subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)) })
    id <- unlist(lapply(complete_data, function(data){ data$ID[[1]] }))
    nobs <- unlist(lapply(complete_data, nrow))

    data.frame(id, nobs)
}
