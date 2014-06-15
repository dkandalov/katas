complete = function(directory, id = 1:332) {
    pad_to_3 = function(x) { formatC(x, width = 3, flag = 0) }
    as_file_name = function(x) { paste0(pad_to_3(x), ".csv") }
    file_names = sapply(id, as_file_name)
    file_paths = sapply(file_names, function(x){ paste0(directory, "/", x) })

    all_data = lapply(file_paths, read.csv)
    complete_data = lapply(all_data, function(data){ subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)) })
    id = sapply(all_data, function(data){ data$ID[[1]] })
    nobs = sapply(complete_data, nrow)

    data.frame(id, nobs)
}
