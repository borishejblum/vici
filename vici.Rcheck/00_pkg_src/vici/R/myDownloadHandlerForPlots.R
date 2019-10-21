#'Custom download handler for plots
#'
#'@param name output file name
#'@param plot_obj a plot object to be downloaded
#'
#'
#'@return a \code{ggpubr} plot object
#'
#'@author Boris Hejblum
#'
#'@keywords internal
#'
#'@import ggplot2
#'@import ggpubr
#'@importFrom grDevices dev.off png

myDownloadHandlerForPlots <- function(name, plot_obj, outputArgs = list()){
  downloadHandler(
    filename = name,
    content = function(file){
      png(file, height=5, width=6, res=300, units = "in")
      print(plot_obj)
      dev.off()
    },
    outputArgs = outputArgs
  )
}
