#' Print out Dashboard contents 
#'
#' This function prints out the Dashboard contents of the target workspace.
#'
#' @param analysis The name of the analysis use want to check the Dashboard of.
#' The list of available analyses can be found with \code{availableAnalysis()}.
#'
#' @return The last modified date as a message, followed by the Dashboard 
#' contents of the target workspace.
#'
#' @examples
#' getDashboard("salmon")
#'
#' @export
getDashboard <- function(analysis) {
    
    ## Load the map
    dir <- system.file("extdata", package = "RunTerraWorkflow")
    map <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)
    
    ## Check the input validity
    if (!analysis %in% map$analysis) {
        x <- paste("Workspace for", analysis, "is not available.")
        stop(x)
    } else {
        ind <- which(map$analysis == analysis)
        workspaceNamespace <- map$workspaceNamespace[ind]
        workspaceName <-map$workspaceName[ind]
    }
    
    ## Extract
    resp <- AnVIL::Terra()$getWorkspace(workspaceNamespace, workspaceName)
    parsed <- suppressMessages(jsonlite::fromJSON(httr::content(resp, "text"),
                                                  simplifyVector = FALSE))
    
    desc <- parsed$workspace$attributes$description
    last_modified <- parsed$workspace$lastModified
    date <- substr(last_modified, 1, 10)
    
    y <- paste("This workspace is last modified on", date)
    message(y)
    cat(desc)
}