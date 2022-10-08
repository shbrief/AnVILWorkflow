#' Download output files from Terra
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id with the 'succeeded' status will be used.
#' @param keyword A character string containing a regular expression to be matched
#' in the output file name. Under the default \code{NULL}, all the outputs from
#' the workflow, including log files, will be returned.
#' @param includeMetadata Under the default (\code{FALSE}), metadata files (e.g.
#' \code{stderr, stdout, .log, .sh}), will not be returned.
#' @param dest_dir Path to the directory where downloaded files are saved
#' @param dry To download the output data, set \code{dry = FALSE}.
#'
#' @export
getOutput <- function(workspaceName,
                      submissionId = NULL, 
                      keyword = NULL,
                      includeMetadata = FALSE, 
                      dest_dir = ".",
                      dry = TRUE) {
    
    ## Get the namespaces
    ws_fullname <- .get_workspace_fullname(workspaceName)
    ws_namespace <- unlist(strsplit(ws_fullname, "/"))[1]
    ws_name <- unlist(strsplit(ws_fullname, "/"))[2]
    avworkspace(ws_fullname)
    
    ## List of all the submissions
    submissions <- monitorWorkflow(workspaceName = ws_fullname)
    
    ## If there is no previous submission
    if (nrow(submissions) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    }
    
    ## The most recent submission
    if (is.null(submissionId)) {
        submission <- submissions[1,]
    } else {
        submission <- submissions[submissions$submissionId == submissionId,]
    }
    
    ## Get submissionId
    submissionId <- submission$submissionId
    
    ## Get the list of all outputs
    av_bucket <- avbucket(namespace = ws_namespace,
                          name = ws_name)
    outputs <- avworkflow_files(submissionId = submissionId, 
                                bucket = av_bucket)

    ## Remove metadata files
    if (isFALSE(includeMetadata)) {
        outputs <- .nonMetadataOutputs(workflowOutputs = outputs)
    }
    
    ## Filter with the keyword
    if (!is.null(keyword)) {
        ind <- grep(keyword, outputs$file)
        res <- outputs[ind,,drop = FALSE] # keyword-containing output files
    } else {res <- outputs}
    
    ## Output
    message(paste("Outputs are from the submissionId", submissionId))

    ## Download outputs
    if (isTRUE(dry)) {
        return(res)
    } else {
        # create destination directory
        if (!dir.exists(dest_dir)) {
            message(paste(dest_dir, "is created."))
            dir.create(dest_dir)
        }
        # Download
        lapply(res$path, gsutil_cp, destination = dest_dir)
    }
}
