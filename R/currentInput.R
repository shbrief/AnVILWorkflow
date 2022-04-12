#' Check the current input arguments
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param inputOnly Under the default (\code{TRUE}), the file path to the input
#' files list and the paths to input files will be returned. If it's set 
#' to \code{FALSE}, the whole method configuration will be returned.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @export
currentInput <- function(workspaceName, 
                         inputOnly = TRUE,
                         accountEmail = gcloud_account(), 
                         billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    ## Method configuration
    config <- .getMethodConfig(billingProjectName, workspaceName)

    ## Get the method configuration
    parsed <- avworkflow_configuration_get(
        workflow_namespace = config$namespace,
        workflow_name = config$name,
        namespace = billingProjectName,
        name = workspaceName
    )

    ## Return
    if (isFALSE(inputOnly)) {return(parsed)} else {return(parsed$inputs)}
}
