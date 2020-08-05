#' datum2df
#' @title datum2df
#' @param datum datum
#' @param vars vars
#' @param pfts pfts
#' @param stderr should stderr be returned as well.
#' @return df
#' @author FM
#' @export

datum2df <- function(datum,vars = c("gpp"),pfts = c(2,3,4,17),name = "ref"){
  
  df <- data.frame()
  for (ivar in seq(1,length(vars))){
    
    if(is.null(datum$szpft[[vars[ivar]]])){
      if (vars[ivar] == "par.all") {
        matrix.tmp <- datum$emean[["par.tot"]] - datum$emean[["parup"]] - datum$emean[["par.gnd"]]
      } else if (vars[ivar] == "nir.tot") {
        matrix.tmp <- datum$emean[["rshort"]] - datum$emean[["par.tot"]]/4.6
      } else if (vars[ivar] == "nir.beam") {
        matrix.tmp <- datum$emean[["rshort.beam"]] - datum$emean[["par.beam"]]/4.6
      } else if (vars[ivar] == "nir.diff") {
        matrix.tmp <- datum$emean[["rshort.diff"]] - datum$emean[["par.diff"]]/4.6
      } else if (vars[ivar] == "nirup") {
        matrix.tmp <- datum$emean[["rshortup"]] - datum$emean[["parup"]]/4.6
      } else if (vars[ivar] == "nir.gnd") {
        matrix.tmp <- datum$emean[["rshort.gnd"]] - datum$emean[["par.gnd"]]/4.6
      } else if (vars[ivar] == "nir.all"){
        matrix.tmp <- datum$emean[["rshort"]] - datum$emean[["par.tot"]]/4.6  - (datum$emean[["rshortup"]] - datum$emean[["parup"]]/4.6) - (datum$emean[["rshort.gnd"]] - datum$emean[["par.gnd"]]/4.6)    
      } else if(vars[ivar] == "tir.all"){
        matrix.tmp <- datum$emean[["rlong"]] - datum$emean[["rlongup"]] - datum$emean[["rlong.gnd"]]  
      } else{
        matrix.tmp <- datum$emean[[vars[ivar]]]      
      }

      names(matrix.tmp)  <- 1:length(matrix.tmp)
      df <- rbind(df,
                  data.frame(var = vars[ivar],melt(matrix.tmp) %>% mutate(time = 1:length(matrix.tmp),pft = 18)))
    } else {
      if (vars[[ivar]] %in% c("agb.recr","recr")){
        matrix.tmp <- datum$szpft[[vars[ivar]]][,2,pfts]
      } else {
        matrix.tmp <- datum$szpft[[vars[ivar]]][,12,pfts]
      }
      
      if(is.null(dim(matrix.tmp))){matrix.tmp <- as.matrix(matrix.tmp)}
      colnames(matrix.tmp) <- pfts
      row.names(matrix.tmp)  <- 1:nrow(matrix.tmp)
      df <- rbind(df,
                  data.frame(var = vars[ivar],melt(matrix.tmp) %>% rename(time = Var1,pft = Var2)))
    }
  }
  return(df %>% mutate(simulation = name,month = rep(datum$month,nrow(df)/length(datum$month)),year = rep(datum$year,nrow(df)/length(datum$year))))
}