#' Print out Dashboard contents 
#'
#' This function prints out the Dashboard contents of the target workspace.
#' You can provide either \code{workspaceName} or \code{analysis}. If both 
#' values are provided, this function will use \code{workspaceName} argument
#' over \code{analysis} argument.
#' 
#' @importFrom utils read.table
#'
#' @param workspaceName The name of the workspace you want to get the 
#' overview provided through the Dashboard.
#' @param analysis The name of the analysis use want to check the Dashboard of.
#' The list of available analyses can be found with \code{availableAnalysis()}.
#'
#' @return The last modified date as a message, followed by the Dashboard 
#' contents from the target workspace.
#'
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' getDashboard(analysis = "salmon")
#' getDashboard(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#'
#' @export
getDashboard <- function(workspaceName = "", 
                         analysis = NULL) {
    
    ## Get the ws_namespace from the map
    if (!is.null(analysis)) {
        ## Load the map
        dir <- system.file("extdata", package = "AnVILWorkflow")
        map <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)
        
        ## Check the input validity
        if (!analysis %in% map$analysis) {
            x <- paste("Workspace for", analysis, "is not available.")
            stop(x)
        } else {
            ind <- which(map$analysis == analysis)
            ws_namespace <- map$workspaceNamespace[ind]
            ws_name <-map$workspaceName[ind]
        }
    }
    
    ## Get the ws_namespace from workspaceName input
    if (nzchar(workspaceName)) {
        ws_fullname <- .get_workspace_fullname(workspaceName)
        ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
        ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    }
    
    ## Extract
    resp <- AnVIL::Terra()$getWorkspace(ws_namespace, ws_name)
    parsed <- suppressMessages(jsonlite::fromJSON(httr::content(resp, "text"),
                                                  simplifyVector = FALSE))
    
    desc <- parsed$workspace$attributes$description
    last_modified <- parsed$workspace$lastModified
    date <- substr(last_modified, 1, 10)
    
    y <- paste("This workspace is last modified on", date)
    message(y)
    cat(desc)
}