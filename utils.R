# All utilities

toLog <- function(where, what){
  logg <<- rbind(logg, data.frame(when = Sys.time()
                               , where
                               , what))
}
