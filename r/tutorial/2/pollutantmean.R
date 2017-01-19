pollutantmean = function(directory, pollutant, id = 1:332) {
    pad_to_3 = function(x) { formatC(x, width = 3, flag = 0) }
    as_file_name = function(x) { paste0(pad_to_3(x), ".csv") }
    file_names = sapply(id, as_file_name)
    file_paths = sapply(file_names, function(x){ paste0(directory, "/", x) })
    read_pollutant_data = function(file_name) {
        read.csv(file_name)[pollutant]
    }

    all_data = lapply(file_paths, read_pollutant_data)
    mean(unlist(all_data), na.rm = TRUE)
}
