# LBSPR 0.1.6 

## Bug Fixes
- fixed minor bugs in Shiny app
- fix issue with `plotSize` introduced with latest version of `ggplot2`
- fix typo in vignette causing a warning on CRAN

# LBSPR 0.1.5
Minor patch to comply with new R standard of `class(matrix(1:4,2,2))` = `matrix` and `array`

# LBSPR 0.1.4

## Minor Changes
- Reduce font size for warnings of high F/M etc in `plotSize`, add
argument to control font size, and set `inc.text=FALSE` as default.

# LBSPR 0.1.3 
## Minor Changes
- fix bug in Shiny app reading in data
- modify SPR circle plot used in the Shiny app.

## Bug Fixes

- fix issue in cpp code that was being flagged by CRAN checks
- fix missing variable M-at-length in LBSPRfit function (thanks to Yves Reecht for picking this up)

# LBSPR 0.1.2
## Minor Changes
- add option to specify scales for axes in plotSize
- modify plotTarg function and add features


# LBSPR 0.1.1

## Minor Changes
- 'absel' fitting routine has been coded in cpp with major improvement in speed
- Shiny applications have now been added to the package. Launch the applications using the `Shiny` function: e.g., `Shiny("LBSPR")` or `Shiny("Sim")`
- the relative spawning stock biomass (SSB) is now calculated and reported in the simulation model
- modified the plotSim function so that it now produces several plots including SSB/SSB0 to yield and spr plot 
- split functions in functions.r into separate r files
- updated the vignette

## Bug Fixes
- added packages required by the Shiny applications as dependencies
- fixed minor bug in standard error calculation
- fixed bug in plotEsts when F/M estimates are high

# LBSPR 0.1.0 
The LBSPR package has now been released on CRAN!

See http://adrianhordyk.github.io/LBSPR for information and examples that describe the package.

