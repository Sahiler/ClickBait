#'@export
runExample <- function (){
  shiny::runApp(
    system.file('shiny-apps', 'AnalysisDemo',
                package='RClickbait'))
}
