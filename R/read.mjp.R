#' Read in a Data Set Encrypted with GPG, an MJP team defined function
#' 
#' @param file     Path to the data file encrypted with GNU Privacy Guard (GPG).  This currently works for CSV, SPSS, Stata, Rdata, and rds file types that have been ecrypted using GPG.
#'
#' @param tmp.path  Path to a directory where the data will be temporarily downloaded and decrypted.  WARNING -- This should not be a place on the cloud such as Dropbox; it should be a place on your local hard drive for security reasons.  The default is to create a temporary directory called "mjp.temp" in the user's Documents directory.  The user can specify a different directory in which to create the mjp.temp as a subdirectory.
#' 
#' @return  A data.frame of the data imported.
#' 
#' @examples
#' ## IMPORT AN RDATA FILE TO DEFAULT TEMPORARY DIRECTORY
#'     mydata <- read.mjp(file="~/Documents/dataset.Rdata.gpg")
#'
#' ## IMPORT AN RDATA FILE TO A USER-SPECIFIED TEMPORARY DIRECTORY
#'     mydata <- read.mjp(file="~/Documents/dataset.Rdata.gpg", tmp.path="~/Data/")
#' 
#' @export
######################################################################
## COPIES ENCRYPTED FILES TO THE LOCAL HARD DRIVE IN A TEMPORARY
## DIRECTORY;
## DECRYPTS AND READS IN THE FILE;
## REMOVES THE LOCAL COPIES AND THE TEMPORARY DIRECTORY;
## WRITTEN BY ADAM G. FERA ON 2017-12-20
######################################################################
read.mjp <- function(file, tmp.path=path.expand("~/Documents/")) {
    ## CREATE A NEW DIRECTORY IN WHICH TO SAVE FILES
    tempDir <- paste0(tmp.path, "mjp.temp/")
    dir.create(path=tempDir)
    ## COPY FILE TO HARD DRIVE
    file.copy(from=file, to=tempDir)
    ## EXTRACT NAME OF ENCRYPTED FILE
    crypt <- basename(path=file)
    ## DECRYPT FILE
    require(rcrypt)
    decrypt(input=paste0(tempDir, crypt), verbosity=0)
    ## EXTRACT NAME OF DECRYPTED FILE
    decrypt <- gsub(pattern=".gpg", replacement="", x=crypt)
    ## EXTRACT FILE EXTENSION OF DECRYPTED FILE
    require(tools)
    file.type <- tolower(file_ext(decrypt))
    file.read <- paste0(tempDir, decrypt)
    ## READ IN ENCRYPTED FILE AND ASSIGN TO OBJECT df
    if (file.type=="csv") {
        df <- read.csv(file=file.read)
    } else if (file.type=="sav") {
            require(foreign)
            df <- read.spss(file=file.read, to.data.frame=TRUE)
    } else if (file.type=="dta") {
            require(haven)
            df <- read_dta(file=file.read)
    } else if (file.type=="rdata") {
            df <- get(load(file=file.read))
    } else if (file.type=="rds") {
            df <- readRDS(file=file.read)
    }
    ## REMOVE ENCRYPTED AND DECRYPTED FILES FROM HARD DRIVE
    unlink(x=tempDir, recursive=TRUE)
    return(df)
}
