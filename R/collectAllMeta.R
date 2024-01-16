#' Get AnVIL workspaces
#'
#' Different from \code{\link[AnVIL]{avworkspaces}}https://drive.google.com/drive/u/0/folders/1NNAzcNRBx4nPfcdqjKPeUlVE7lhxXMeL
#' 
#' @import AnVIL
#' 
getAllWorkspaces <- function() {
    response <- Terra()$listWorkspaces()
    
    flatten(response) |> 
        select(name = .data$workspace.name, 
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

#' @import httr
#' @importFrom dplyr full_join
#' 
getAllWorkflows <- function(workspaces = NULL, removeId = TRUE) {
    
    ##<<<<<<<<<<<< Include some input sanity check
    if (is.null(workspaces)) {
        workspaces <- getAllWorkspaces()
    }
    
    ## Add temporary `id` column for table combining
    workspaces$id <- paste0(workspaces$namespace, "/", workspaces$name)
    allCombined <- data.frame()

    for (i in seq_len(nrow(workspaces))) {
        
        ## Get information on workflows for each workspaces
        workflows <- Rawls()$list_method_configurations(
            workspaces$namespace[i], 
            URLencode(workspaces$name[i]), 
            TRUE
        ) |>
            flatten()
        
        ## Combine workflows 
        colnames(workflows)[1:2] <- paste0("wf_", colnames(workflows)[1:2])
        workflows$id <- paste0(workspaces$namespace[i], "/", workspaces$name[i])
        
        combined <- dplyr::full_join(workspaces, workflows, by = "id")
        allCombined <- plyr::rbind.fill(allCombined, combined)
    }
    
    if (isTRUE(removeId)) {combined <- select(combined, -id)}
    return(allCombined)
}


#' @import httr
#' @importFrom dplyr full_join
#' 
#' 
getAllDataTables <- function(workspaces = NULL) {
    
    ##<<<<<<<<<<<< Include some input sanity check
    if (is.null(workspaces)) {
        workspaces <- getAllWorkspaces()
    }
    
    ## Add temporary `id` column for table combining
    workspaces$id <- paste0(workspaces$namespace, "/", workspaces$name)

    dt_colnames <- c("namespace", "name", "table", "count", "colnames")
    allCombined <- as.data.frame(matrix(nrow = 0, ncol = length(dt_colnames)))
    colnames(allCombined) <- dt_colnames
    
    for (i in seq_len(nrow(workspaces))) {
        
        namespace <- workspaces$namespace[i]
        name <- workspaces$name[i]
        
        res <- avtables(namespace = namespace, name = name)
        res$namespace <- namespace
        res$name <- name
        allCombined <- plyr::rbind.fill(allCombined, res)
    }
    
    return(allCombined)
}

