fun_freqsum_table <- function(dataset, 
                              path_report_output,
                              x = 20,
                              sort_by = "COUNT",
                              sort_descending = T) {
    # Empty the csv file.
    tmp_csv <- file(path_report_output, open = "w")
    truncate(tmp_csv)
    
    # make sure the datset is a data.frame
    dataset <- data.frame(dataset)
    
    for (i in 1:ncol(dataset)) {
        write.table(
            data.frame(i ,
                       names(dataset)[i],
                       class(dataset[, i])),
            path_report_output,
            sep = ",",
            row.names = F,
            col.names = F,
            append = T
        )
        
        if (is.factor(dataset[, i])) {
            tmp_sum <- freqsum(dataset[, i], x = x, sort_by = sort_by , sort_descending = sort_descending)
        } else if (any(is.integer(dataset[, i]),
                       is.numeric(dataset[, i]))) {
            tmp_sum <-
                as.data.frame(unclass(summary(dataset[, i])))
            tmp_sum[, 2] <- tmp_sum[, 1]
            tmp_sum[, 1] <- rownames(tmp_sum)
            names(tmp_sum) <-
                c("Descriptive" , names(dataset)[i])
        } else if (is.POSIXct(dataset[, i])) {
            tmp_sum <-
                as.data.frame(unclass(summary(dataset[, i])))
            tmp_sum[, 2] <- tmp_sum[, 1]
            tmp_sum[, 1] <- rownames(tmp_sum)
            tmp_sum[, 2] <-
                as.POSIXct(tmp_sum[, 2], origin = ymd("1970-01-01"))
            names(tmp_sum) <-
                c("Descriptive" , names(dataset)[i])
        } else if (any(is.logical(dataset[, i]),
                       is.character(dataset[, i]))) {
            tmp_sum <- freqsum(as.factor(dataset[, i]))
        }
        
        write.table(
            tmp_sum,
            path_report_output,
            sep = ",",
            row.names = F,
            col.names = T,
            append = T
        )
        write.table(
            matrix("", 2, 4),
            path_report_output,
            sep = ",",
            row.names = F ,
            col.names = F,
            append = T
        )
    }
    
}