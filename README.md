## AnVILWorkflow

For R users with the limited computing resources, we introduce AnVILWorkflow 
package. This package allows users to run workflows implemented in 
[Terra](https://app.terra.bio/#) without writing any workflow, installing 
softwares, or managing cloud resources. Terra's computing resources rely on 
Google Cloud Platform (GCP) and to use AnVILWorkflow, you only need to 
setup the Terra account once at the beginning.

Using [AnVIL](https://github.com/Bioconductor/AnVIL) package, AnVILWorkflow 
allows users to access both Terra and GCP through R session from a conventional 
laptop, greatly lowers the learning curve for high-performance, cloud-based 
genomics resources.

<img src="https://raw.githubusercontent.com/shbrief/AnVILWorkflow/master/inst/extdata/runnable_workflow.png?token=ADX67SQPCAGDGWQZCGQMIBTAJGHKC" width="90%" height="90%"/>


#### Example: microbiome analysis
[bioBakery workflows](https://github.com/biobakery/biobakery_workflows) is a 
collection of workflows and tasks for executing common microbial community 
analyses using standardized, validated tools and parameters. bioBakery is 
built and maintained by [Huttenhower lab](http://huttenhower.sph.harvard.edu/).
