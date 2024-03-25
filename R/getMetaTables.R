# Returning AnVIL workspace urls
.wsURL <- function(workspace_namespace, workspace_name) {
    workspace_url <- file.path("https://app.terra.bio/#workspaces",
                               URLencode(workspace_namespace), 
                               URLencode(workspace_name))
    return(workspace_url)
}

#' Creates a metadata table of all workspaces
#' 
#' Metadata of all the GCP-based workspaces are collected.
#' 
#' @import dplyr
#' @importFrom tidyr unnest
#'
#' @export
getWorkspaces <- function() {
    response <- Terra()$listWorkspaces()
    response <- as.list(response)
    dat <- cbind(response$workspace, 
                 accessLevel = response$accessLevel, 
                 public = response$public) %>%
        filter(cloudPlatform == "Gcp") # <<<<<<<<<<<<<<<<<< Only GCP now
    workspaces <- dat %>%
        as_tibble(.name_repair = "universal") %>%
        tidyr::unnest(attributes) %>%
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
        tidyr::unnest(datatype) %>% 
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


#' @importFrom rlang is_scalar_character
#' 
.avSampleData <- function(namespace = avworkspace_namespace(), 
                          name = avworkspace_name()) {
    
    ## Sanity check
    stopifnot(rlang::is_scalar_character(namespace), 
              rlang::is_scalar_character(name))
    
    ## Columns/Attributes for curation
    dir <- system.file("extdata", package = "AnVILWorkflow")
    curated_cols <- readLines(file.path(dir, "curated_cols.txt"))
    
    res <- avtables(namespace, name) # a table of metadata tables
    set_ind <- grep("_set$", res$table) # remove metadata tables for set
    res <- res[-set_ind,]
    res[, curated_cols] <- NA # add the curated_cols to the master table
    
    for (i in seq_len(nrow(res))) {
        avtb <- avtable(as.character(res$table[i]), namespace, name)
        colnames(avtb) <- toupper(colnames(avtb))
        
        ## If the metadata table contains any of the to-be-curated columns
        columns <- intersect(curated_cols, colnames(avtb))
        for (column in columns) {
            
            ## Gender/Sex Curation: Convert to descriptive values
            sex_cols <- c("SEX_CALL", "SEX", "GENDER")
            if (column %in% sex_cols) {
                tb_with_sex <- which(!is.na(avtb[column]))
                sex_vals <- unlist(avtb[, column]) %>%
                    gsub("^M$|1|^male$", "Male", .) %>%
                    gsub("^F$|2|^female$", "Female", .) %>% 
                    ifelse(is.na(.), "NA", .) %>%
                    table()
                vals <- .tableToString(sex_vals)
                res[i, column] <- vals
            } else {
                ## Save all the unique values from the to-be-curated columns
                vals <- paste(unique(unlist(avtb[, column])), collapse = "<;>")
                res[i, column] <- vals
            }
        }
    }
    return(res)
}


#' Summary table into a single string where the column name and the value
#' is separated by `sep` and columns are separated by `delim`
#' 
#' @param tb A table. Output from the `table` function. 
#' @param sep A delimiter to separate the column name and value
#' @param delim A delimiter to separate columns
#' 
.tableToString <- function(tb, sep = ":", delim = ";") {
    attr <- names(tb)
    attr_num <- as.character(tb)
    res <- paste(attr, attr_num, sep = ":") %>% 
        paste(., collapse = ";")
    return(res)
}
    
#' Apply to the returned value from `.avSampleData` function
#' 
#' @importFrom rlang is_scalar_character
#' @param sampleData A data frame. 
#' 
.getAgeMinMax <- function(sampleData) {
    
    ## Initialize age range columns
    sampleData$AGE_MIN <- NA
    sampleData$AGE_MAX <- NA
    
    ## Identify min and max ages
    age_cols <- c("AGE", "PFB:AGE_AT_INDEX", "AGE_RANGE", 
                  "DONOR_ORGANISM__ORGANISM_AGE")
    
    for (age_col in age_cols) {
        tb_with_age <- which(!is.na(sampleData[age_col]))
        for (i in tb_with_age) {
            ages <- strsplit(as.character(sampleData[i, age_col]), 
                             split = "<;>|-| year") %>% unlist
            ages <- ages[ages != "NA"] %>% as.numeric
            sampleData$AGE_MIN[i] <- min(ages)
            sampleData$AGE_MAX[i] <- max(ages)
        }
    }
    return(sampleData)
}  


#' Creates a metadata table of data from all workspaces provided
#' 
#' This function usually takes a long time to run due to the large volume
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
        print(msg)
        
        workspace_namespace <- allWorkspaces$workspace_namespace[i]
        workspace_name <- allWorkspaces$workspace_name[i]
        res <- .avSampleData(namespace = workspace_namespace,
                             name = workspace_name) %>% 
            .getAgeMinMax(.)
        
        res$workspace_key <- allWorkspaces$workspace_key[i]
        data <- plyr::rbind.fill(data, res)
    }
    
    # ## Add the name_key
    # data$name_key <- paste(data$name, data$workspace_key, sep = ":")
    return(data)
}