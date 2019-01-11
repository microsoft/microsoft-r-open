<center>
<img alt="vscode logo" src="https://mran.microsoft.com/assets/img/ClarkHead.png" width="100px">
</center>
Microsoft R Open
================

Microsoft R Open is the enhanced distribution of R used for for statistical analysis and Data science.This is the repository containing the source code for the open source components of Microsoft R Open. If you're looking for the latest changes, check the "public" branch.


Directory Structure
-------------------

/additionalPackages -> Microsoft authored GPLv2 licensed R packages

/patch              -> Patches made to the R source code when building Microsoft R Open

/source             -> CRAN R source code

/vendor             -> Libraries needed to build Microsoft R Open



Building
--------

Please see the [R Installation and Administration Guide](https://cran.r-project.org/doc/manuals/r-release/R-admin.html) for instructions on building the R source. Patches can be applied using GNU Patch.

The additional Microsoft authored packages can be built and installed using R CMD INSTALL after the R source has been built.

Learn more about Microsoft R open at <https://mran.microsoft.com/>.
