## RunTerraWorkflow

For R users with limited computing resources, we introduce RunTerraWorkflow package. 
This package allows users to run [Terra](https://app.terra.bio/#) implemented workflows 
on Google Cloud Platform (GCP) without writing any workflow, installing softwares, or 
managing cloud resources. GCP resources are managed by Terra, a cloud-based genomics 
platform, and users only need to setup their account only once at the beginning to 
use RunTerraWorkflow.

Using [AnVIL](https://github.com/Bioconductor/AnVIL) package, RunTerraWorkflow allows
users to access both Terra and GCP through R session from a conventional laptop, greatly
lowers the learning curve for high-performance, cloud-based genomics resources.

<img src="https://raw.githubusercontent.com/shbrief/RunTerraWorkflow/master/inst/extdata/runnable_workflow.png" width="90%" height="90%"/>

You can find out how to use this package [here](https://rpubs.com/shbrief/RunTerrraWorkflow_bioBakery).   
