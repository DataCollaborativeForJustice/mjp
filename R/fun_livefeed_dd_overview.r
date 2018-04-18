#' Auto-generate the Overview table of MOCJ livefeed data dictionary
#' 
#' @param dataset_nm_keyword     String, The keyword which will be used to search for the dataset
#' @param db_connect T/F value, indicate whether the Database info (data type and length in DB) is desired.
#' @param db_username     The username for logging into MOCJ livefeed database
#' @param db_password      The password for logging into MOCJ livefeed database
#' 
#' @return  A data frame containning the following information of each variable in all tables: Table name, Column index, Variable name, Variable description (blank), PII indecater(Blank), Data type in database, length in database, data type in R, number of rows, missing count and missing percentage.
#' 
#' @details This function will search for the dataset with the given keyword in the global enviroment. Thus, loading all of the dataset into the memory and naming them with the same exact keyword is requried. \n The variable description and PII indecator in the output are used as place holder, user has to either link with other references or manually input.
#' 
#' @examples
#'     fun_freqsum_table(my_df , "C:\\Users\\tlin\\Dropbox\\Richie\\RFiles\\summary.csv")
#'     fun_freqsum_table(my_df , "C:\\Users\\tlin\\Dropbox\\Richie\\RFiles\\summary.csv", x = 50,  sort_by = "VARIABLE", sort_descending = T)
#' @export


fun_livefeed_dd_overview <-
    function(dataset_nm_keyword ,
             db_connect = F,
             db_username = as.character(),
             db_password = as.character()) {
        rdata_name <- grep(dataset_nm_keyword , ls(envir = .GlobalEnv), value = T)
        
        if (db_connect) {
            channel <-
                odbcConnect(
                    "anaprd",
                    uid = db_username,
                    pwd = db_password,
                    believeNRows = FALSE
                )
            
            db_info_tables <-
                sqlTables(channel, schema = "ANA_MOCJ")
            
            missing_count <- data.frame(
                Table = as.character(),
                Col_Index = as.numeric(),
                Variable = as.character(),
                Description = as.character(),
                PII = as.logical(),
                Data_Type_in_DB = as.character(),
                Length_in_DB = as.integer(),
                Data_Type_in_R = as.character(),
                Number_of_rows = as.numeric(),
                Missing_count = as.numeric(),
                Missing_perc = as.numeric()
            )
            
            
            for (j in rdata_name) {
                tmp_df <- as.data.frame(get(j))
                db_col_info <-
                    sqlColumns(channel, paste0("WC_V_", j))
                
                for (i in 1:length(names(tmp_df))) {
                    missing_count <- rbind(
                        missing_count,
                        data.frame(
                            Table = j,
                            Col_Index = i,
                            Variable = names(tmp_df)[i],
                            Description = as.character(NA),
                            PII = as.logical(NA),
                            Data_Type_in_DB = db_col_info[i, "TYPE_NAME"],
                            Length_in_DB = db_col_info[i, "COLUMN_SIZE"],
                            Data_Type_in_R = as.character(class(tmp_df[, i])[1]),
                            Number_of_rows = length(tmp_df[, i]),
                            Missing_count = sum(is.na(tmp_df[, i])),
                            Missing_perc = sum(is.na(tmp_df[, i])) / nrow(tmp_df)
                        )
                    )
                }
                
            }
            close(channel)
        } else {
            missing_count <- data.frame(
                Table = as.character(),
                Col_Index = as.numeric(),
                Variable = as.character(),
                Description = as.character(),
                PII = as.logical(),
                Data_Type_in_R = as.character(),
                Number_of_rows = as.numeric(),
                Missing_count = as.numeric(),
                Missing_perc = as.numeric()
            )
            
            for (j in rdata_name) {
                tmp_df <- as.data.frame(get(j))
                for (i in 1:length(names(tmp_df))) {
                    missing_count <- rbind(
                        missing_count,
                        data.frame(
                            Table = j,
                            Col_Index = i,
                            Variable = names(tmp_df)[i],
                            Description = as.character(NA),
                            PII = as.logical(NA),
                            Data_Type_in_R = as.character(class(tmp_df[, i])[1]),
                            Number_of_rows = length(tmp_df[, i]),
                            Missing_count = sum(is.na(tmp_df[, i])),
                            Missing_perc = sum(is.na(tmp_df[, i])) / nrow(tmp_df)
                        )
                    )
                }
            }
        }
        return(missing_count)
        
    }
