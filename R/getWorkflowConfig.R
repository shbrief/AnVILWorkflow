#' Check the workflow configuration
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param workflowName Name of the workflow to run. If a single workflow is  
#' available under the specified workspace, this function will check the input
#' of that workflow under the default (\code{NULL}). If there are multiple 
#' workflows available, you should specify the workflow. 
#'
#' @return A data.frame for the inputs defined in a workflow configuration. 
#' 
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' config <- getWorkflowConfig(workspaceName = "Bioconductor-Workflow-DESeq2")
#' config
#' }
#' 
#' @export
getWorkflowConfig <- function(workspaceName, 
                              workflowName = NULL) {
    
    setCloudEnv(message = FALSE)
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    wf_fullname <- .get_workflow_fullname(workspaceName = workspaceName,
                                          workflowName = workflowName)
    wf_namespace <- unlist(strsplit(wf_fullname, "/"))[1]
    wf_name <- unlist(strsplit(wf_fullname, "/"))[2]
    
    ## Get workflow configuration
    config <- avworkflow_configuration_get(
        workflow_namespace = wf_namespace,
        workflow_name = wf_name,
        namespace = ws_namespace,
        name = ws_name
    )
    
    return(config)
}