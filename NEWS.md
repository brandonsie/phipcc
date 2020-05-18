# phipcc 0.1.4 (2020-05-18 Monday)  
- Slightly simplified default template used by `render_from_template()`. Former default template can be used with `phipcc::render_from_template(template = system.file("template_case_control_larman.Rmd", package = "phipcc"))`.
- Initial version of `config_case_control()` implemented to write config.tsv parameter file from R.

# phipcc 0.1.3 (2020-04-14 Tuesday)
- added pep_aa term to plan target data_annotated to allow custom column name for amino acid sequence.

# phipcc 0.1.2
- added remove\_leading\_x parameter to fix naming issue for sample IDs starting with a number .E.g. X101.Sample Name is converted to 101.Sample.Name.

# phipcc 0.1.1  
## Minor changes  
- Exported necessary data for template to an .Rdata file in /data/ subdirectory. This allows targets to be generated on one computer (e.g. on a high performance cluster) and the report to be generated locally after copying the /data/ directory and running `phipcc::render_from_template(import_rdata == TRUE)`.


# phipcc 0.1.0  
## Major changes  
- AVARDA incorporation