#' Abort submitted job
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId A character. Submission ID you want to abort. You can find
#' the submission id using \code{monitorSubmission} function. If it is not defined,
#' the most recent submission will be aborted.
#'
#' @export
abortSubmission <- function(accountEmail, billingProjectName, workspaceName,
                            submissionId = NULL) {

    ## Setup gcloud account/project
    .set_gcloud(accountEmail, billingProjectName)

    ## List of all the submissions
    submissions <- monitorSubmission(accountEmail,
                                     billingProjectName,
                                     workspaceName)

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
        message(paste0("Status of the submitted job (submissionId: ", submissionId, ")"))
        resp <- Terra()$abortSubmission(workspaceNamespace = billingProjectName,
                                        workspaceName = workspaceName,
                                        submissionId = submissionId)
        if (resp$status_code == 204) {print("Workflow is succesfully aborted.")}
        if (resp$status_code == 401) {print("You are not authorized to access.")}
        if (resp$status_code == 404) {print("Submission is not found.")}
        if (resp$status_code == 500) {print("Internet Error.")}
    }
}
