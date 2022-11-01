##### Function Overview Table ##################################################
## Table for the manuscript
dt <- data.frame(Steps = c("Prepare","","Run","","","Result"), 
                 Functions = c("cloneWorkspace","updateInput","runWorkflow", 
                               "monitorWorkflow", "stopWorkflow",
                               "getOutput"),
                 Description = c("Copy the template workspace",
                                 "Update the workflow inputs",
                                 "Launch the workflow in Terra",
                                 "Monitor the status of your workflow run",
                                 "Abort the submission",
                                 "List or download your workflow outputs"))

library(kableExtra)
dir <- "~/Packages/AnVILWorkflow/inst/extdata"
dt %>%
    kbl(caption = "Table 1. Major functions to run Terra workflow") %>%
    kable_classic(full_width = FALSE, html_font = "Verdanaf") %>%
    save_kable(file.path(dir, "Table1.html"))
webshot::webshot("inst/extdata/Table1.html",
                 "inst/extdata/Table1.pdf")
