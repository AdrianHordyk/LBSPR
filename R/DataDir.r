#' Report the location of the Data Files
#'
#' A function that returns the location of the example CSV files
#'
#' @author A. Hordyk modified (i.e., stolen) from T. Carruthers' code (DLMtool package)
#' @export
DataDir<-function(){
    return(paste(searchpaths()[match("package:LBSPR",search())],"/",sep=""))
}
