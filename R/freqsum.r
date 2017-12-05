freqsum <-
    function(InputCol,
             x = 20,
             sort_by = "COUNT",
             sort_descending = T) {
        tmpvar <- data.frame(Freq = (summary(as.factor(InputCol), x)))
        tmpvar <- cbind(TempVar = row.names(tmpvar), tmpvar)
        row.names(tmpvar) <- c()
        tmpvar$Percentage <-
            paste(round(tmpvar$Freq / sum(tmpvar$Freq) * 100, 4), "%")
        names(tmpvar) <- c("VARIABLE", "COUNT", "PERCENTAGE")
        tmpvar_no_oth_na <-
            tmpvar[!(tmpvar$VARIABLE %in% c("(Other)", "NA's")), ]
        if (sort_by == "COUNT") {
            tmpvar_no_oth_na <-
                tmpvar_no_oth_na[order(as.numeric(tmpvar_no_oth_na[, sort_by]), decreasing = sort_descending), ]
        } else{
            tmpvar_no_oth_na <-
                tmpvar_no_oth_na[order(as.character(tmpvar_no_oth_na[, sort_by]), decreasing = sort_descending), ]
        }
        tmpvar_no_oth_na <- rbind(tmpvar_no_oth_na,
                                  tmpvar[tmpvar$VARIABLE == "(Other)", ],
                                  tmpvar[tmpvar$VARIABLE == "NA's", ])
        tmpvar <- tmpvar_no_oth_na
        row.names(tmpvar) <- c(1:nrow(tmpvar))
        return(tmpvar)
    }