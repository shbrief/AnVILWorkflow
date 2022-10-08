#' Check the status of submitted jobs
#'
#' @import AnVIL
#'
#' @param workspaceName Character(1). Name of the workspace
#'
#' @return A tibble summarizing submitted workflow jobs. Contains information 
#' such as submission Id, submission date, and submission status.
#'
#' @export
monitorWorkflow <- function(workspaceName) {

    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName = workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    
    # ## Check provided namespace is correct
    # if (!is.null(workspaceNamespace)) {
    #     stopifnot(ws_namespace == workspaceNamespace)
    # } 
    
    ## Get the submission summary
    res <- avworkflow_jobs(namespace = ws_namespace, name = ws_name)
    return(res)
}
