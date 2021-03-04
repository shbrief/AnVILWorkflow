#' Launch Terra workflow
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param useCallCache A logical. Under the default condition (\code{TRUE}), call
#' cache will be used.
#' @param inputName Name of you input entity. If the workflow is using Terra's
#' data model, this is required. The available entities can be found using the
#' \code{findInputName} function.
#'
#' @export
launchWorkflow <- function(accountEmail, billingProjectName, workspaceName,
                           useCallCache = TRUE,
                           inputName = NULL) {

    ## Setup gcloud account/project
    .set_gcloud(accountEmail, billingProjectName)

    ## Method configuration
    configuration <- .getMethodConfig(billingProjectName, workspaceName)

    ## rootEntityName (if rootEntityType is assigned)
    if (!is.null(configuration$rootEntityType)) {
        if(is.null(inputName)) {
            stop("Provide your input's name. You can look it up using findInputName() function.")
        } else {
            rootEntityName <- inputName
        }
    } else {
        rootEntityName <- NULL
    }

    ## Submit
    resp <- Terra()$createSubmission(
        workspaceNamespace = billingProjectName,
        workspaceName = workspaceName,
        methodConfigurationNamespace = configuration$namespace,
        methodConfigurationName = configuration$name,
        entityType = configuration$rootEntityType,
        entityName = rootEntityName,
        useCallCache = useCallCache)

    if (resp$status_code == 201) {
        print("Workflow is succesfully launched.")
    } else {
        print(paste("Workflow launching is failed.", ))
    }
}
