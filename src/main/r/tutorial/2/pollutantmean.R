pollutantmean <- function(directory, pollutant, id = 1:332) {
    join <- function(...){ paste(..., sep = "") }
    pad_to_3 <- function(x) { formatC(x, width = 3, flag = 0) }
    append_csv <- function(x) { join(x, ".csv") }
    file_names <- lapply(pad_to_3(id), append_csv)
    file_paths <- lapply(file_names, function(x){ join(directory, "/", x) })

    all_data = lapply(file_paths, read.csv)
    pollutant_data <- lapply(all_data, function(data){ na.omit(data[pollutant]) })

    mean(unlist(pollutant_data))
}
