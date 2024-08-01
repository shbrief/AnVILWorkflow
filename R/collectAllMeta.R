#' Get AnVIL workspaces
#'
#' Different from \code{\link[AnVILGCP:avworkspace-methods]{avworkspaces}}
#' 
#' @import AnVIL
#' 
#' @examples
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' allWorkspaces <- getAllWorkspaces()
#' }
#' 
getAllWorkspaces <- function() {
    response <- AnVIL::Terra()$listWorkspaces()
    
    flatten(response) |> 
        select(workspaceId = workspace.workspaceId,
               name = .data$workspace.name, 
               namespace = .data$workspace.namespace,
               public = public,
               lastModified = .data$workspace.lastModified, 
               createdBy = .data$workspace.createdBy, 
               accessLevel = .data$accessLevel,
               indication = `workspace.attributes.library:indication`,
               study_design = `workspace.attributes.library:studyDesign`,
               subject_count = `workspace.attributes.library:numSubjects`,
               disease_site = `workspace.attributes.library:primaryDiseaseSite`,
               country = `workspace.attributes.library:cohortCountry`,
               project = `workspace.attributes.library:projectName`,
               reference_genome = `workspace.attributes.library:reference`) |> 
        mutate(name = trimws(.data$name), 
               lastModified = as.Date(.data$lastModified)) |> 
        arrange(.data$name, desc(.data$lastModified))
} 

#' Collect workflows from all workspaces a user has access to
#' 
#' @import httr
#' @importFrom dplyr full_join
#' @importFrom utils URLencode
#' 
#' @param workspaces Under the default (\code{NULL}), workflows from all the 
#' workspaces a user has access to will be collected.
#' 
#' @examples
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' allWorkflows <- getAllWorkflows()
#' }
#' 
#' @export
getAllWorkflows <- function(workspaces = NULL) {
    
    ##<<<<<<<<<<<< Include some input sanity check
    if (is.null(workspaces)) {
        workspaces <- getAllWorkspaces()
    }
    
    allCombined <- data.frame()

    for (i in seq_len(nrow(workspaces))) {
        
        ## Get information on workflows for each workspaces
        workflows <- AnVIL::Rawls()$list_method_configurations(
            workspaces$namespace[i], 
            URLencode(workspaces$name[i]), 
            TRUE
        ) |>
            flatten()
        
        ## Skip if there is no workflow in a workspace
        if (nrow(workflows) == 0) {next}
        
        ## Assign workspaceId as a primary key
        workflows$workspaceId <- workspaces$workspaceId[i]
        
        ## Combine workflows 
        colnames(workflows)[1:2] <- paste0("wf_", colnames(workflows)[1:2]) # workflow name and namespace

        combined <- dplyr::full_join(workspaces, workflows, by = "workspaceId")
        allCombined <- plyr::rbind.fill(allCombined, combined)
    }
    
    return(allCombined)
}


#' Get all the data tables
#' 
#' @import httr
#' @importFrom dplyr full_join
#' 
#' @param workspaces A character vector. Under the default (\code{NULL}), 
#' all the data tables from all the workspaces user has access to will be 
#' returned. If you specify this, the data tables only from the specified 
#' workspace(s) will be returned.
#' 
#' @return A Data Frame of all the data tables
#'  
#' @examples
#' library(AnVIL)
#' if (gcloud_exists() && nzchar(avworkspace_name())) {
#' allDataTables <- getAllDataTables()
#' }
#' 
#' @export
getAllDataTables <- function(workspaces = NULL) {
    
    ##<<<<<<<<<<<< Include some input sanity check
    if (is.null(workspaces)) {
        workspaces <- getAllWorkspaces()
    }

    dt_colnames <- c("workspaceId", "namespace", "name", 
                     "table", "count", "colnames")
    allCombined <- as.data.frame(matrix(nrow = 0, ncol = length(dt_colnames)))
    colnames(allCombined) <- dt_colnames
    
    for (i in seq_len(nrow(workspaces))) {
        
        namespace <- workspaces$namespace[i]
        name <- workspaces$name[i]
        workspaceId <- workspaces$workspaceId[i]
        
        res <- avtables(namespace = namespace, name = name)
        res$workspaceId <- workspaceId
        res$namespace <- namespace
        res$name <- name
        allCombined <- plyr::rbind.fill(allCombined, res)
    }
    
    return(allCombined)
}

