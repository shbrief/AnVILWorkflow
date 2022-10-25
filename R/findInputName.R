#' Find the root entity name
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param rootEntity A character. Type of root entity for Terra's data model.
#' For example, \code{participant}, \code{participant_set}, \code{sample}, etc.
#' @param nameOnly Under the default (\code{TRUE}), only the names of a given
#' root entity type will be returned.
#' 
#' @return A character vector of input names under the given root entity.
#'
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' .findInputName(
#' workspaceName = "Bioconductor-Workflow-DESeq2",
#' rootEntity = "participant_set")
#' }
#'
.findInputName <- function(workspaceName, 
                           rootEntity = "", 
                           nameOnly = TRUE) {
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    
    ## Available data model
    tb_all <- avtables(namespace = ws_namespace,
                       name = ws_name)
    
    ## Validate rootEntity input
    if (!nzchar(rootEntity)) {
        message("Please select the rootEntity from the followings:")
        print(tb_all$table)
        .stop_quietly()
    } else if (!rootEntity %in% tb_all$table) {
        message("The provided rootEntity doesn't exist. Please provide the correct rootEntity from the followings:")
        print(tb_all$table)
        .stop_quietly()
    }
    
    ## Data model table
    tb <- avtable(table = rootEntity,
                  namespace = ws_namespace,
                  name = ws_name)

    if (isTRUE(nameOnly)) {
        res <- as.list(tb)[[1]]
        return(res)
    } else {return(tb)}
}
