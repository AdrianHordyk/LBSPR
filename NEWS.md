# LBSPR 0.1.3 
## Minor Changes
- fix bug in Shiny app reading in data
- modify SPR circle plot used in the Shiny app.

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

