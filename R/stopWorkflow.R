#' Abort submitted job
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param submissionId A character. Submission ID you want to abort. You can find
#' the submission id using \code{monitorSubmission} function. If it is not defined,
#' the most recent submission will be aborted.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param dry Logical(1) when `TRUE` (default), report the consequences but do 
#' not perform the action requested. When `FALSE`, perform the action.
#'
#' @export
stopWorkflow <- function(workspaceName,
                         submissionId = NULL,
                         accountEmail = gcloud_account(), 
                         billingProjectName = gcloud_project(),
                         dry = TRUE) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    ## List of all the submissions
    submissions <- avworkflow_jobs(namespace = billingProjectName,
                                   name = workspaceName)

    ## The most recent submission
    if (is.null(submissionId)) {
        submissionId <- .mostRecentSubmissionId(billingProjectName,
                                                workspaceName)
    }

    submission <-  submissions[submissions$submissionId == submissionId,]
    if (submission$status == "Done") {
        res <- which(submission == 1)
        print(paste0("Submitted job (submissionId:", submissionId,
                     ") is already done: Job was ", names(submission)[res]))
    } else if (submission$status %in% c("Aborted", "Aborting")) {
        print(paste0("Submitted job (submissionId:", submissionId,
              ") is already aborted."))
    } else {
        message(paste0("Status of the submitted job (submissionId: ", 
                       submissionId, ")"))
        
        # Didn't use avworkflow_stop to get the status code
        rawls <- Rawls()
        resp <- rawls$abortSubmission(workspaceNamespace = billingProjectName, 
                                     workspaceName = workspaceName, 
                                     submissionId = submissionId)
        
        if (resp$status_code == 204) {print("Workflow is succesfully aborted.")}
        if (resp$status_code == 401) {print("You are not authorized to access.")}
        if (resp$status_code == 404) {print("Submission is not found.")}
        if (resp$status_code == 500) {print("Internet Error.")}
    }
}
