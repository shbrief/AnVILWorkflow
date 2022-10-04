#' Get the workflow namespace and name
#' 
#' Use this internally when \code{\link{setCloudEnv}} is already run.
#' 
#' @param workspaceName A character. Name of the workspace to use. 
#' @param workflowName A character. Name of the workflow to run. If a single
#' workflow is available under the selected workspace, this function will
#' check the input of that workflow under the default (\code{NULL}). If there
#' are multiple workflows available, you should specify the workflow. 
#' 
#' @return A character of \code{"workflow_namespace/workflow_name"}
#'
.get_workflow_fullname <- function(workspaceName,
                                   workflowName = NULL) {
    
    ws_fullname <- .get_workspace_fullname(ws_name = workspaceName)
    
    ## Get all the available workflow
    res <- avworkflows(namespace = unlist(strsplit(ws_fullname, split = "/"))[1],
                       name = unlist(strsplit(ws_fullname, split = "/"))[2])
    
    ## Select a workflow
    if (nrow(res) == 0) {
        stop("This workspace does not have any workflow.")
    } else if (nrow(res) == 1) {
        wf_fullname <- paste(res$namespace, res$name, sep = "/")
    } else if (is.null(workflowName)) {
        warning("Please specify the workflowName from the following: ")
        print(res)
        stop()
    } else {
        ind <- which(res$name == workflowName)
        wf_fullname <- paste(res$namespace[ind], res$name[ind], sep = "/")
    }
    
    return(wf_fullname)
}
