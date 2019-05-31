
# phipmake 0.1.1  
## Minor changes  
Exported necessary data for template to an .Rdata file in /data/ subdirectory. This allows targets to be generated on one computer (e.g. on a high performance cluster) and the report to be generated locally after copying the /data/ directory and running `phipcc::render_from_template(import_rdata == TRUE)`.


# phipmake 0.1.0  
## Major changes  
AVARDA incorporation