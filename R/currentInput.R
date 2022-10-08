#' Check the current input arguments
#'
#' @import AnVIL
#' @param config Terra workflow configuration. Output from the
#' \code{avworkflow_configuration_get()} function.
#'
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
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param requiredInputOnly Under the default (\code{TRUE}), only the required
#' inputs are returned.
#' @param analysis If specified, only the minimally required inputs for a 
#' given workflow will be returned. 
#'
#' @return A data.frame for the inputs defined in a workflow configuration. 
#' 
#' @examples 
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' currentInput(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#' 
#' @export
currentInput <- function(workspaceName, 
                         workflowName = NULL,
                         accountEmail = gcloud_account(), 
                         billingProjectName = gcloud_project(),
                         requiredInputOnly = TRUE,
                         analysis = NULL) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    ## Get workflow namespace
    wf_fullname <- .get_workflow_fullname(workspaceName = workspaceName,
                                          workflowName = workflowName)
    wf_fullname_split <- unlist(strsplit(wf_fullname, "/"))
    
    ## Get workflow configuration
    config <- avworkflow_configuration_get(
        workflow_namespace = wf_fullname_split[1],
        workflow_name = wf_fullname_split[2],
        namespace = avworkspace_namespace(),
        name = workspaceName
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
