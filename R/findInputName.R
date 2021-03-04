#' Find the root entity name
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param rootEntity A character. Type of root entity for Terra's data model.
#' @param nameOnly Under the default (\code{TRUE}), only the names of a given
#' root entity type will be returned.
#'
#' @export
findInputName <- function(accountEmail, billingProjectName, workspaceName,
                          rootEntity, nameOnly = TRUE) {

    ## Setup gcloud account/project
    .set_gcloud(accountEmail, billingProjectName)

    ## Data model table
    tb <- avtable(table = rootEntity,
                  namespace = billingProjectName,
                  name = workspaceName)

    if (isTRUE(nameOnly)) {
        res <- tb$participant_set_id
        return(res)
    } else {return(tb)}
}
