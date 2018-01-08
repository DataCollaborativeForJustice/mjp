#' Frequency summary, a MJP team defined function
#' 
#' @param InputCol     Input column or vector, will be convert into factor.
#' @param x number of categories shown in the summary table, default = 20.
#' @param sort_by      sort summary table by "VARIABLE" = category lable or "COUNT" = freqency
#' @param sort_descending     show the result in acsending or descending order, default = TRUE, in descending order.
#' @param big_mark adding seperator in the big numbers. if you dont like the scientific notation when using this on large numbers, please change your globle enviromental variable with 'option(scipen = 999)' .
#' 
#' @return     A frequency summary table with percentage.
#' 
#' @examples
#'     freqsum(my_df$col1)
#'     freqsum(my_df$col2, x = 50, sort_by = "VARIABLE", sort_descending = T)
#' @export
freqsum <-
    function(InputCol,
             x = 20,
             sort_by = "COUNT",
             sort_descending = T,
             big_mark = NA) {
        if (any(is.integer(InputCol),
                is.numeric(InputCol))) {
            tmp_sum <-
                as.data.frame(unclass(summary(InputCol)))
            tmp_sum[, 2] <- tmp_sum[, 1]
            tmp_sum[, 1] <- rownames(tmp_sum)
            names(tmp_sum) <-
                c("Descriptive" , "Quantile")
            out <- tmp_sum
        }
        else if (lubridate::is.POSIXct(InputCol)) {
            tmp_sum <-
                as.data.frame(unclass(summary(InputCol)))
            tmp_sum[, 2] <- tmp_sum[, 1]
            tmp_sum[, 1] <- rownames(tmp_sum)
            tmp_sum[, 2] <-
                as.POSIXct(tmp_sum[, 2], origin = ymd("1970-01-01"))
            names(tmp_sum) <-
                c("Descriptive" , "Quantile")
            out <- tmp_sum
        }
        else{
            tmpvar <- data.frame(Freq = (summary(as.factor(InputCol), x)))
            tmpvar <- cbind(TempVar = row.names(tmpvar), tmpvar)
            row.names(tmpvar) <- c()
            tmpvar$Percentage <-
                paste(round(tmpvar$Freq / sum(tmpvar$Freq) * 100, 4), "%")
            names(tmpvar) <-
                c("VARIABLE", "COUNT", "PERCENTAGE")
            tmpvar_no_oth_na <-
                tmpvar[!(tmpvar$VARIABLE %in% c("(Other)", "NA's")),]
            if (sort_by == "COUNT") {
                tmpvar_no_oth_na <-
                    tmpvar_no_oth_na[order(as.numeric(tmpvar_no_oth_na[, sort_by]), decreasing = sort_descending),]
            }
            else{
                tmpvar_no_oth_na <-
                    tmpvar_no_oth_na[order(as.character(tmpvar_no_oth_na[, sort_by]),
                                           decreasing = sort_descending),]
            }
            tmpvar_no_oth_na <- rbind(tmpvar_no_oth_na,
                                      tmpvar[tmpvar$VARIABLE == "(Other)",],
                                      tmpvar[tmpvar$VARIABLE == "NA's",])
            tmpvar <- tmpvar_no_oth_na
            if(!is.na(big_mark)){
                tmpvar$COUNT <- format(tmpvar$COUNT, big.mark = big_mark)
            }
            row.names(tmpvar) <- c(1:nrow(tmpvar))
            out <- tmpvar
        }
        
        return(out)
    }
