#' Download output files from Terra
#'
#' @import AnVILGCP
#' @importFrom jsonlite fromJSON 
#' @importFrom utils stack
#'
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id with the 'succeeded' status will be used.
#' @param keyword A character string containing a regular expression to be 
#' matched in the output file name. Under the default \code{NULL}, all the 
#' outputs from the workflow, including log files, will be returned.
#' @param dest_dir Path to the directory where downloaded files are saved
#' @param dry To download the output data, set \code{dry = FALSE}.
#' 
#' @return If \code{"dry=TRUE"}, this function will return a data frame with
#' two columns named 'filename' and 'name'.
#' \itemize{
#'     \item \code{filename}: Name of the actual output files.
#'     \item \code{name}: Name of the output defined in your workflow script.
#'     This is how configuration refers the outputs.
#' }
#'
#' @examples 
#' library(AnVILBase)
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#' getOutput(workspaceName = "Bioconductor-Workflow-DESeq2")
#' }
#' 
#' @export
getOutput <- function(workspaceName,
                      submissionId = NULL, 
                      keyword = NULL,
                      # includeMetadata = FALSE, 
                      dest_dir = ".",
                      dry = TRUE) {
    
    setCloudEnv(message = FALSE)
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    avworkspace(ws_fullname)
    
    ## List of all the submissions
    submissions <- monitorWorkflow(workspaceName = ws_fullname)
    success_ind <- which(submissions$succeeded == 1)
    
    ## If there is no previous submission
    if (nrow(submissions) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    } else if (length(success_ind) == 0) {
        stop("None of the previously submitted job succeeded.", call. = FALSE)
    }
    
    ## The most recent submission
    if (is.null(submissionId)) {
        latest_success_ind <- success_ind[1]
        submission <- submissions[latest_success_ind,] #<<<<<<<<<<<<<<<<< This is taken care by `avworkflow_files` 
    } else {
        submission <- submissions[submissions$submissionId == submissionId,]
    }
    
    ## Pause if no output is created
    if (submission$status != "Done") {
        msg <- paste("No output available: Your submission",
                     submission$submissionId, "is", submission$status)
        stop(msg)
    } else if (submission$failed == 1) {
        msg <- paste("No output available: Your submission",
                     submission$submissionId, "is failed.")
        stop(msg)
    }
    
    ## Get submissionId
    submissionId <- submission$submissionId
    
    ##### Get the output name and workflowId
    ## Get workflow full name
    wf_fullname <- .get_workflow_fullname(workspaceName = workspaceName)
    wf_namespace <- unlist(strsplit(wf_fullname, "/"))[1]
    wf_name <- unlist(strsplit(wf_fullname, "/"))[2]
    
    ## Get the output configuration
    config <- avworkflow_configuration_get(
        workflow_namespace = wf_namespace,
        workflow_name = wf_name,
        namespace = ws_namespace,
        name = ws_name
    )
    out_config <- avworkflow_configuration_outputs(config)
    
    ## Get the workflowId for `Terra()$workflowOutputsInSubmission`
    res1 <- AnVIL::Terra()$monitorSubmission(workspaceNamespace = ws_namespace,
                                      workspaceName = ws_name, 
                                      submissionId = submissionId)
    workflowId <- jsonlite::fromJSON(rawToChar(res1$content))$workflow$workflowId #<<<<<<<<<<<<<<<<<< This might not available for `failed` workflows
    
    ## All outputs
    res2 <- AnVIL::Terra()$workflowOutputsInSubmission(
        workspaceNamespace = ws_namespace,
        workspaceName = ws_name,
        submissionId = submissionId,
        workflowId = workflowId
    )
    outputs <- fromJSON(rawToChar(res2$content))
    
    ## Make a named (=output name) list of outputs (= output file name)
    all_output_fnames <- vector(mode = "list", 
                                length = length(out_config$name))
    for (i in seq_along(out_config$name)) {
        ind <- grep(out_config$name[i], names(unlist(outputs))) 
        output_fnames <- unlist(outputs)[ind]
        
        names(all_output_fnames)[i] <- out_config$name[i]
        all_output_fnames[[i]] <- c(output_fnames)
    }
        
    output_df <- utils::stack(all_output_fnames)
    rownames(output_df) <- NULL
    names(output_df) <- c("filename", "name")

    # ## Remove metadata files
    # if (isFALSE(includeMetadata)) {
    #     outputs <- .nonMetadataOutputs(workflowOutputs = outputs)
    # }

    ## Filter with the keyword
    # if (!is.null(keyword)) {
    #     ind <- grep(keyword, outputs$file)
    #     res <- outputs[ind,,drop = FALSE] # keyword-containing output files
    # } else {res <- outputs}
    
    if (!is.null(keyword)) {
        ind <- grep(keyword, output_df$filename)
        res <- output_df[ind,,drop = FALSE]
    } else {res <- output_df}
    
    ## Output
    msg <- paste("Outputs are from the submissionId", submissionId)
    message(msg)

    ## Download outputs
    if (isTRUE(dry)) {
        res$filename <- basename(res$filename)
        return(res)
    } else {
        # create destination directory
        if (!dir.exists(dest_dir)) {
            msg <- paste(dest_dir, "is created.")
            message(msg)
            dir.create(dest_dir)
        }
        # Download
        lapply(res$filename, gsutil_cp, destination = dest_dir)
    }
}
