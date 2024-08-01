#' Check the status of submitted jobs
#'
#' @import AnVILGCP
#'
#' @param workspaceName Character(1). Name of the workspace
#'
#' @return A tibble summarizing submitted workflow jobs. Contains information 
#' such as submission Id, submission date, and submission status.
#'
#' @examples 
#' library(AnVILBase)
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#' monitorWorkflow(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#'
#' @export
monitorWorkflow <- function(workspaceName) {

    setCloudEnv(message = FALSE)
    
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
