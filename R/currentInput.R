#' Check the current input arguments
#'
#' @import AnVIL
#' @param config Terra workflow configuration. Output from the
#' \code{avworkflow_configuration_get()} function.
#' 
#' @return A list length of two, including inputListPath and inputFilePath.
#' 
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' config <- avworkflow_configuration_get(
#' workflow_namespace = "mtx_workflow_biobakery_version3", 
#' workflow_name = "mtx_workflow_biobakery_version3", 
#' workspace_namespace = "waldronlab-terra-rstudio", 
#' workspace_name = "mtx_workflow_biobakery_version3_template")
#' biobakery_inputs <- .biobakery_currentInput(config)
#' }
#'
#' @keywords internal
.biobakery_currentInput <- function(config) {
    
    res <- config$inputs$workflowMTX.inputRead1Files
    if (nzchar(res)) {
        res <- gsub("\"", "", res)
        structure(list(inputListPath = res,
                       inputFilePath = AnVIL::gsutil_cat(res)))
    } else {
        return("Input files are not provided.")
    }
}


#' Check the current input arguments
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param workflowName Name of the workflow to run. If a single workflow is  
#' available under the specified workspace, this function will check the input
#' of that workflow under the default (\code{NULL}). If there are multiple 
#' workflows available, you should specify the workflow. 
#' @param requiredInputOnly Under the default (\code{TRUE}), only the required
#' inputs are returned.
#' @param analysis If specified, only the minimally required inputs for a 
#' given workflow will be returned. 
#'
#' @return A data.frame for the inputs defined in a workflow configuration. 
#' 
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' currentInput(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#' 
#' @export
currentInput <- function(workspaceName, 
                         workflowName = NULL,
                         requiredInputOnly = TRUE,
                         analysis = NULL) {

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
    
    ## Get input
    input <- avworkflow_configuration_inputs(config)

    ## Required/Optional inputs
    if (isTRUE(requiredInputOnly)) {
        ind <- which(input$optional == FALSE)
        input <- input[ind,]
    }
    
    ## Inputs for specific analysis
    if (!is.null(analysis)) {
        if (analysis == "bioBakery") {
            input <- .biobakery_currentInput(config)
        } else {
            message(paste(analysis, "doesn't provide a minimal input list."))
        }
    }
    
    ## Return inputs
    return(input)
}
