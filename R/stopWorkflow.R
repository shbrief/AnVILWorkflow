#' Abort submitted job
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param submissionId A character. Submission ID you want to abort. You can 
#' find the submission id using \code{monitorWorkflow} function. If it is 
#' not defined, the most recent submission will be aborted.
#' @param dry Logical(1) when `TRUE` (default), report the consequences but do 
#' not perform the action requested. When `FALSE`, perform the action.
#'
#' @return This function will print out whether the call for workflow 
#' abortion was successful or not. In case it was unsuccesful, the diagnosis
#' will be suggested as a part of the message.
#' 
#' @examples 
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' if ("salmon" %in% avworkspaces()$name)
#' stopWorkflow(workspaceName = "salmon")
#' }
#'
#' @export
stopWorkflow <- function(workspaceName,
                         submissionId = NULL,
                         dry = TRUE) {

    setCloudEnv(message = FALSE)
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]

    ## List of all the submissions
    submissions <- monitorWorkflow(workspaceName = workspaceName)
    
    ## If there is no previous submission
    if (nrow(submissions) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    }

    ## The most recent submission
    if (is.null(submissionId)) {
        submission <- submissions[1,]
    } else {
        submission <- submissions[submissions$submissionId == submissionId,]
    }
    
    ## Get submissionId
    submissionId <- submission$submissionId
    
    ## Describe submission status
    if (submission$status == "Done") {
        res <- which(submission == 1)
        show(paste0("Submitted job (submissionId:", submissionId,
                    ") is already done: Job was ", names(submission)[res]))
    } else if (submission$status %in% c("Aborted", "Aborting")) {
        show(paste0("Submitted job (submissionId:", submissionId,
             ") is already aborted."))
    } else {
        show(paste0("Status of the submitted job (submissionId: ", 
                    submissionId, ")"))
        
        # Didn't use avworkflow_stop to get the status code
        rawls <- Rawls()
        resp <- rawls$abortSubmission(workspaceNamespace = ws_namespace, 
                                      workspaceName = ws_name, 
                                      submissionId = submissionId)
        
        if (resp$status_code == 204) {show("Workflow is succesfully aborted.")}
        if (resp$status_code == 401) {show("You are not authorized to access.")}
        if (resp$status_code == 404) {show("Submission is not found.")}
        if (resp$status_code == 500) {show("Internet Error.")}
    }
}
