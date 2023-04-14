#' Find the available analysis
#'
#' This function shows the available analyses and the brief
#' descriptions of them.
#' 
#' @importFrom utils read.table
#' @importFrom AnVIL avworkspaces
#'
#' @param curatedOnly Default is \code{TRUE}, returning only workspaces that
#' offer simplified input configuration by this package. If it is set to 
#' \code{FALSE}, all the workspaces 
#' @param keyword Default is \code{NULL}. When this argument is provided as 
#' a character(1), it will return only the workspaces containing the keyword
#' and the user has an access to.
#'
#' @return A data frame. The \code{analysis} columns shows the name of the
#' available analyses, which is the required input (\code{analysis} argument)
#' for the functions implemented in AnVILWorkflow package.
#'
#' @examples
#' availableAnalysis()
#'
#' @export
availableAnalysis <- function(curatedOnly = TRUE,
                              keyword = NULL) {
    
    setCloudEnv(message = FALSE)
    
    if (isTRUE(curatedOnly)) {
        dir <- system.file("extdata", package = "AnVILWorkflow")
        res <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)

    } else {
        all_ws <- avworkspaces()
        res <- data.frame(namespace = all_ws$namespace,
                          name = all_ws$name)
    }
    
    if (is.null(keyword)) {
        return(res)
    } else {
        ind <- grep(keyword, res$name, ignore.case = TRUE)
        res <- res[ind,,drop = FALSE]
        return(res)
    }
}

