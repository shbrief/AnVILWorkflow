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
    focal_cols <- c("COUNTRY",
                    "COUNTRY_OF_RECRUITMENT",
                    "LIBRARY_LAYOUT",
                    "LIBRARY_STRATEGY",
                    "LIBRARY_SOURCE",
                    "LIBRARY-1_NAME",
                    "LIBRARY_PREP_KIT_METHOD",
                    "LIBRARY_CONSTRUCTION_PROTOCOL", 
                    "SEQUENCING_PROTOCOL__INSTRUMENT_MANUFACTURER_MODEL",
                    "ET_PILOT_PLATFORMS", 
                    "LC_PILOT_PLATFORMS", 
                    "MAIN_PROJECT_E_PLATFORM", 
                    "MAIN_PROJECT_LC_PLATFORM", 
                    "PHASE1_E_PLATFORM",
                    "PHASE1_LC_PLATFORM",
                    "PLATFORM", 
                    "PLATFORM_NAME", 
                    "INSTRUMENT", 
                    "INSTRUMENT_MODEL", 
                    "INSTRUMENT_PLATFORM",
                    "SEQ_PLATFORM",
                    "SEQUENCING_PLATFORM",
                    "SEQUENCING_PLATFORM_MODEL",
                    "PFB:STUDY_DESCRIPTION",
                    "DESCRIPTION",
                    "DISEASE_MORPHOLOGY",
                    "PHENOTYPE_DESCRIPTION",
                    "CORONARY_ARTERY_DISEASE",
                    "SPECIMEN_FROM_ORGANISM__ORGAN", 
                    "SPECIMEN_FROM_ORGANISM__ORGAN_PART", 
                    "TISSUE_SOURCE",
                    "SPECIES", 
                    "DONOR_ORGANISM__GENUS_SPECIES", 
                    "GENOTYPHI_SPECIES", 
                    "SONNEITYPING_SPECIES", 
                    "ANI_TOP_SPECIES_MATCH",
                    "AGE_AT_INDEX",
                    "AGE", 
                    "PFB:AGE_AT_INDEX", 
                    "AGE_RANGE", 
                    "DONOR_ORGANISM__ORGANISM_AGE",
                    "COHORT", 
                    "GENDER",
                    "SEX",
                    "SEX_CALL",
                    "REPORTED_GENDER", 
                    "REPORTED_SEX", 
                    "PFB:ANNOTATED_SEX", 
                    "HAS_PHENOTYPIC_SEX",
                    "DONOR_ORGANISM__SEX",
                    "POPULATION", 
                    "POPULATION_LABEL", 
                    "SUBPOPULATION", 
                    "SUPERPOPULATION",
                    "PFB:POPULATION",
                    "POPULATION_DESCRIPTION", 
                    "SUPER_POPULATION_DESCRIPTION",
                    "POPULATION_ID")
    
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
    
    # Initialize age range columns
    avdata$age_min <- NA
    avdata$age_max <- NA
    
    # Identify min and max value for each table
    for (col in c("AGE", "PFB:AGE_AT_INDEX", "AGE_RANGE", "DONOR_ORGANISM__ORGANISM_AGE")){
      for (table in which(!is.na(avdata[,col]))){
        ages <- list()
        for (i in unlist(strsplit(as.character(avdata[table,col]), split="<;>|-| year"))){
          if (i!="NA"){
            ages <- append(ages, as.numeric(i))
          }
        }
        ages <- data.frame(all_age = unlist(ages))
        avdata$age_min[table] <- min(ages$all_age)
        avdata$age_max[table] <- max(ages$all_age)
      }
    }
    
    # Gender/Sex Curation: Convert to descriptive values
    avdata$SEX_CALL[which(avdata$SEX_CALL=="F<;>M<;>UKN")] <- "Female<;>Male<;>Unknown"
    avdata$SEX[which(avdata$SEX=="1<;>2" | avdata$SEX=="M<;>F")] <- "Male<;>Female"
    avdata$GENDER[which(avdata$GENDER=="1<;>2")] <- "Male<;>Female"
    
    # Disease Curation: Convert to descriptive values
    avdata$CORONARY_ARTERY_DISEASE[which(!is.na(avdata$CORONARY_ARTERY_DISEASE))] <- "Coronary Artery Disease<;>No"
    
    avdata$workspace_key <- paste(namespace, name, sep = ":") 
    return(avdata)
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