# Launch the ShinyApp (Do not remove this comment)
# To build Shiny App, in RStudio use the Run App button (Green triangle) above this file
# To deploy, run: rsconnect::deployApp() 
# Or in RStudio use the blue circular button above this file to Publish Application

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

FEGSDR::fegs_app() # add parameters here (if any)