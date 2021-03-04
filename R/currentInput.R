#' Check the current input arguments
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param inputOnly Under the default (\code{TRUE}), the file path to the input
#' files list and the paths to input files will be returned. If it's set to \code{FALSE},
#' the whole method configuration will be returned.
#'
#' @export
currentInput <- function(accountEmail, billingProjectName, workspaceName,
                         inputOnly = TRUE) {

    ## Setup gcloud account/project
    .set_gcloud(accountEmail, billingProjectName)

    ## Method configuration
    config <- .getMethodConfig(billingProjectName, workspaceName)

    ## Get the method configuration
    parsed <- avworkflow_configuration(
        configuration_namespace = config$namespace,
        configuration_name = config$name,
        namespace = billingProjectName,
        name = workspaceName
    )

    ## Return
    if (isFALSE(inputOnly)) {return(parsed)} else {return(parsed$inputs)}
}
