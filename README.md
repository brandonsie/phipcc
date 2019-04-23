
install_github("brandonsie/epitopefindr")
install_github("brandonsie/phipmake")
install_github("ropensci/drake")
install_github("brandonsie/phipcc")



remove(list = ls())
setwd("D:/RData/cc_test_1/")
unloadNamespace("phipcc")
library(phipcc)
library(drake)
library(magrittr)
drake::expose_imports("phipcc")
plan <- phipcc::define_plan_case_control()
# drake::outdated(drake::drake_config(plan))
drake::make(plan)



config <- readd(config)
candidate_table_flagged <- readd(candidate_table_flagged_html)
candidate_table_full <- readd(candidate_table_html)
graphs <- list(readd(plot1), readd(plot2), readd(plot3))
cluster1 <- readd(clustergram1)
cluster2 <- readd(clustergram2)

phipcc::render_from_template(
  set_title = paste0(
    config$value[config$param == "proj_id"],
    " PhIP-Seq Case-Control Report: ",
    config$value[config$param == "library"],
    " Library"
  ),
  config = config,
  candidate_table_flagged = candidate_table_flagged,
  candidate_table_full = candidate_table_full,
  graphs = graphs,
  cluster1 = cluster1,
  # motifs = "D:/RData/cc_test_1/data/epitopefindr/intermediate_files/msa/",
  motifs = paste0(getwd(), "/data/epitopefindr/intermediate_files/msa/")
  cluster2 = cluster2
)
