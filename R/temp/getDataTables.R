#' Check the current input arguments
#'
#' @import AnVIL
#' 
#' @param workspaceName Character(1). Name of the workspace that contains 
#' the workflow(s) you want to launch.
#' @param table Character(1). Any value under the \code{table} column of the 
#' output with the default \code{table = NULL} can be used.
#' 
#' @return A data frame. It contains all the data table(s) associated with 
#' the workspace/table.
#' 
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' getDataTables(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#' 
#' @export
getDataTables <- function(workspaceName, table = NULL) {
    
    setCloudEnv(message = FALSE)
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    
    if (is.null(table)) {
        res <- avtables(name = workspaceName)
    } else {
        res <- avtable(name = workspaceName, table = table)
    }
    
    return(res)
}