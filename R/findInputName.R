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
.findInputName <- function(workspaceName, 
                           rootEntity, 
                           nameOnly = TRUE) {
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]

    ## Available data model
    tb_all <- avtables(namespace = ws_namespace,
                       name = ws_name)
    ## Data model table
    tb <- avtable(table = rootEntity,
                  namespace = ws_namespace,
                  name = ws_name)

    if (isTRUE(nameOnly)) {
        res <- as.list(tb)[[1]]
        return(res)
    } else {return(tb)}
}
