#' Clone template workspace
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace you are creating
#' @param analysis Name of the analysis you want to clone it's workspace. The
#' list of available analyses can be found using \code{availableAnalysis()}.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @return Name of the cloned workspace
#'
#' @export
cloneWorkspace <- function(workspaceName,  
                           analysis,
                           accountEmail = gcloud_account(), 
                           billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    ## Get the WorkspaceNamespace and WorkspaceName of the template
    dir <- system.file("extdata", package = "RunTerraWorkflow")
    map <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)
    ind <- which(map$analysis == analysis)

    ## Clone the workspace
    res <- try(avworkspace_clone(namespace = map$workspaceNamespace[ind],  # template WorkspaceNamespace
                                 name = map$workspaceName[ind],  # template WorkspaceName
                                 to_namespace = billingProjectName,
                                 to_name = workspaceName),
               silent = TRUE)

    if (methods::is(res, "try-error")) {
        return(message(res))
    } else {return(res)}
}
