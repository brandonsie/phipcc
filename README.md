

```
install_github("brandonsie/epitopefindr")
install_github("brandonsie/phipmake")
install_github("ropensci/drake")
install_github("brandonsie/phipcc")

```


```
library(phipcc)
library(drake)
library(magrittr)
drake::expose_imports("phipcc")
plan <- phipcc::define_plan_case_control()
drake::make(plan)
phipcc::render_from_template()

```

