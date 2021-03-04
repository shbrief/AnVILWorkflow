#' Check the status of submitted jobs
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#'
#' @return A tibble summarizing submitted workflow jobs. Contains information such
#' as submission Id, submission date, and submission status.
#'
#' @export
monitorSubmission <- function(accountEmail, billingProjectName, workspaceName) {

    ## Setup gcloud account/project
    .set_gcloud(accountEmail, billingProjectName)

    res <- avworkflow_jobs(namespace = billingProjectName,
                           name = workspaceName)

    if (length(res) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    } else {return(res)}
}
