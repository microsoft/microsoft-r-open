Microsoft R Open
================

This is the repository containing the source code for the open source components of Microsoft R Open.


Directory Structure
-------------------

/additionalPackages -> GPLv2 licensed R packages not part of the R source

/patch              -> Patches made to the R source code when building Microsoft R Open

/source             -> CRAN R source code

/vendor             -> Libraries needed to build Microsoft R Open



Building
--------

Please see the [R Installation and Administration Guide](https://cran.r-project.org/doc/manuals/r-release/R-admin.html) for instructions on building the R source. Patches can be applied using GNU Patch.

The additional Microsoft authored packages can be built and installed using R CMD INSTALL after the R source has been built.