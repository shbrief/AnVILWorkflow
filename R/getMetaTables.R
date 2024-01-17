# Returning AnVIL workspace urls
.wsURL <- function(workspace_namespace, workspace_name) {
    workspace_url <- paste0("https://app.terra.bio/#workspaces/", 
                            URLencode(workspace_namespace), 
                            "/", 
                            URLencode(workspace_name))
    return(workspace_url)
}

#' Creates a metadata table of all workspaces
#'
#' @export
getWorkspaces <- function() {
    response <- Terra()$listWorkspaces()
    response <- as.list(response)
    dat <- cbind(response$workspace, 
                 accessLevel = response$accessLevel, 
                 public = response$public)
    workspaces <- dat %>%
        as_tibble(.name_repair = "universal") %>%
        unnest(attributes) %>%
        select(workspace_namespace = namespace,
               workspace_name = name,
               accessLevel = accessLevel,
               public = public,
               workspace_is_locked = isLocked,
               indication = "library:indication",
               study_design = "library:studyDesign",
               cohort_country = "library:cohortCountry",
               description = "description",
               dataset_owner = "library:datasetOwner",
               dataset_custodian = "library:datasetCustodian",
               dataset_description = "library:datasetDescription",
               number_of_subjects = "library:numSubjects",
               primary_disease_site = "library:primaryDiseaseSite",
               project_name = "library:projectName",
               cell_type = "library:cellType",
               data_use_restriction = "library:dataUseRestriction",
               reference = "library:reference",
               dataset_name = "library:datasetName",
               datatype = "library:datatype") %>% 
        mutate(workspace_url = .wsURL(workspace_namespace, workspace_name),
               .after = 3) %>% 
        mutate(idx = row_number(), .before = 1)
    
    workspaces <- as.data.frame(workspaces) %>% 
        unnest(datatype) %>% 
        select(-itemsType) %>% 
        rename(data_type = items)
    
    workspaces$data_type <- sapply(workspaces$data_type, 
                                   function(x) paste(unlist(unique(x)), 
                                                     collapse = ";"))
    workspaces$workspace_key <- paste(workspaces$workspace_namespace, 
                                      workspaces$workspace_name, 
                                      sep = "/")
    return(workspaces)
}

#' Creates a metadata table of workflows from all workspaces provided
#' 
#' @param allWorkspaces A data frame of all the workspaces you have access
#' to. An output from the \code{getWorkspaces} function.
#'
#' @export
getWorkflows <- function(allWorkspaces){

    allWorkflows <- data.frame()
    
    for (i in seq_len(nrow(allWorkspaces))) {
        msg <- paste(i, "out of", nrow(allWorkspaces))
        print(i)
        workspace_namespace <- allWorkspaces$workspace_namespace[i]
        workspace_name <- allWorkspaces$workspace_name[i]
        res <- avworkflows(namespace = workspace_namespace,
                           name = workspace_name) 

        res$workspace_key <- allWorkspaces$workspace_key[i]
        allWorkflows <- plyr::rbind.fill(allWorkflows, res)
    }
    
    ## Add the name_key
    allWorkflows$name_key <- paste(allWorkflows$name, 
                                   allWorkflows$workspace_key, 
                                   sep = ":")
    
    ## Formatting (not sure this is necessary)
    colnames(allWorkflows) <- gsub("^methodRepoMethod\\.", "", 
                                   colnames(allWorkflows))
    
    return(allWorkflows)
}


avsampledata <-function(namespace = avworkspace_namespace(), 
                        name = avworkspace_name()) {
    
    stopifnot(is_scalar_character(namespace), is_scalar_character(name))
    focal_cols <- c("PLATFORM", 
                    "PLATFORM_NAME", 
                    "PFB:ORGAN", 
                    "INSTRUMENT", 
                    "INSTRUMENT_MODEL", 
                    "LIBRARY_LAYOUT", 
                    "LIBRARY_CONSTRUCTION_PROTOCOL", 
                    "LIBRARY_STRATEGY", 
                    "DESCRIPTION", 
                    "AGE", 
                    "COHORT", 
                    "GENDER")
    
    avdata <- avtables(namespace, name)
    avdata[, focal_cols] <- NA
    
    for (i in seq_len(nrow(avdata))) {
        avtb <- avtable(as.character(avdata$table[i]), 
                        namespace, 
                        name)
        colnames(avtb) <- toupper(colnames(avtb))
        
        columns <- intersect(focal_cols, colnames(avtb))
        for (column in columns){
            vals <- paste(unlist(unique(avtb[, column])), collapse = "<;>")
            avdata[i, column] <- vals
        }
    }
    
    avdata$workspace_key <- paste(namespace, name, sep = ":") 
    return(avdata)
}


#' Creates a metadata table of data from all workspaces provided
#' 
#' This function usually takes a long time to run due to the large volumn
#' of AnVIL data.
#' 
#' @param allWorkspaces A data frame of all the workspaces you have access
#' to. An output from the \code{getWorkspaces} function.
#'
#' @export
getData <- function(allWorkspaces){
    
    data <- data.frame()
    for (i in seq_len(nrow(allWorkspaces))) {
        msg <- paste(i, "out of", nrow(allWorkspaces))
        print(i)
        
        workspace_namespace <- allWorkspaces$workspace_namespace[i]
        workspace_name <- allWorkspaces$workspace_name[i]
        res <- avsampledata(namespace = workspace_namespace,
                            name = workspace_name) 
        
        res$workspace_key <- allWorkspaces$workspace_key[i]
        data <- plyr::rbind.fill(data, res)
    }
    
    ## Add the name_key
    data$name_key <- paste(data$name, data$workspace_key, sep = ":")
    return(data)
}