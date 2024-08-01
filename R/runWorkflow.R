#' Launch Terra workflow
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace that contains the workflow(s) you
#' want to launch.
#' @param config Workflow configuration. Output from the 
#' \code{\link{getWorkflowConfig}} function.
#' @param workflowName Name of the workflow to run. If this input is not
#' provided but there is only a single workflow available, the function 
#' will automatically use the only workflow.
#' @param useCallCache A logical. Under the default condition (\code{TRUE}), 
#' call cache will be used.
#' @param inputName Name of you input entity. If the workflow is using Terra's
#' data model, this is required. The available entities can be found using the
#' \code{findInputName} function.
#' 
#' @return This function will print out whether the call for workflow 
#' launching was successful or not. 
#'
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' if ("salmon" %in% avworkspaces()$name)
#' runWorkflow(workspaceName = "salmon")
#' }
#'
#' @export
runWorkflow <- function(workspaceName,
                        config,
                        workflowName = NULL,
                        useCallCache = TRUE,
                        inputName = NULL) {
    
    setCloudEnv(message = FALSE)
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    
    # wf_fullname <- .get_workflow_fullname(workspaceName = workspaceName,
    #                                       workflowName = workflowName)
    # wf_namespace <- unlist(strsplit(wf_fullname, "/"))[1]
    # wf_name <- unlist(strsplit(wf_fullname, "/"))[2]
    # 
    # config <- avworkflow_configuration_get(
    #     workflow_namespace = wf_namespace,
    #     workflow_name = wf_name,
    #     namespace = ws_namespace,
    #     name = ws_name
    # )
    
    ## rootEntityName (if rootEntityType is assigned)
    if (!is.null(config$rootEntityType)) {
        if(is.null(inputName)) {
            inputNames <- findInputName(workspaceName = ws_name,
                                        rootEntity = config$rootEntityType)
            if (length(inputNames) == 1) {
                rootEntityName <- inputName
            } else {
                message("You should provide the inputName from the followings:")
                show(inputNames)
                stop()
            }
        } else {
            rootEntityName <- inputName
        }
    } else {
        rootEntityName <- NULL
    }
    
    # avworkflow_run(config, name = workspaceName) <<<<<<<<<<<<<<<<<<<<<<<<<<<<
    resp <- AnVIL::Terra()$createSubmission(
        workspaceNamespace = ws_namespace,
        workspaceName = ws_name,
        methodConfigurationNamespace = config$namespace,
        methodConfigurationName = config$name,
        entityType = config$rootEntityType,
        entityName = rootEntityName,
        useCallCache = useCallCache)
    
    if (resp$status_code == 201) {
        show("Workflow is succesfully launched.")
    } else {
        res <- jsonlite::fromJSON(httr::content(resp, "text"))
        msg <- paste("Workflow launching is failed:", res[[1]])
        show(msg)
    }
}
