# phipcc <img src="https://brandonsie.github.io/docs/phipcc.png" align="right" width="140">

R package to build case-control reports from [PhIP-Seq](https://www.nature.com/articles/s41596-018-0025-6) data. `phipcc` depends on multiple other packages including [`drake`](https://github.com/ropensci/drake), [`phipmake`](https://github.com/brandonsie/phipmake), [`epitopefindr`](https://github.com/brandonsie/epitopefindr). Familiarity with these packages will be useful in working with `phipcc`.

# Setup  

1. In R console, execute:  
``` r
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("ropensci/drake")
remotes::install_github("brandonsie/phipmake")
remotes::install_github("brandonsie/epitopefindr")
remotes::install_github("brandonsie/phipcc")

```
## Additional epitopefindr-specific setup steps 
1. Install a TeX distribution with `pdflatex`. (e.g. [MiKTeX (version 2.9+)](https://miktex.org)). _(Optional; used to convert multiple sequence alignment TeX files to PDF.)_  
2. Install [pdftk (version 2.02+)](https://www.pdflabs.com/tools/pdftk-server/). _(Optional; used to merge individual PDFs into a single file.)_


# Usage

``` r
# Load and attach main libraries
library(phipcc)
library(drake)
library(magrittr)

# Setup phipcc configuration file
# A template is provided with field descriptions. For now, you will need to edit fields manually.
file.copy(system.file("example_config.tsv", package = "phipcc"), "config.tsv")

# Prepare drake plan
drake::expose_imports("phipcc")
plan <- phipcc::define_plan_case_control(config_name = "config.tsv")

# Build targets specified in plan
drake::make(plan)

# Incorporate targets from drake into R Markdown report
# Should be called from the same working directory as drake::make(plan)
phipcc::render_from_template()

```

