#' datum2df_patch
#' @title datum2df
#' @param datum datum
#' @param vars vars
#' @param patches pfts
#' @return df
#' @author FM
#' @export

datum2df_patch <- function(datum, vars = c("gpp","npp","agb.change","lai.change","agb","lai"),patches){
  
  
  if (patches == "uniform"){
    
    df_sum2 <- data.frame()
    
    for (ivar in seq(1,length(vars))){
      
      cvar <- vars[ivar]
      if (cvar == "agb.change"){
        cvar = "agb"
      } else if (cvar == "lai.change"){
        cvar = "lai"
      }
      
      dbh <- unlist(datum$cohort$dbh)
      pft <- c(unlist(datum$cohort$pft))
      if(cvar %in% c("agb","npp")){
        pos <- as.vector(which((pft == 17 & dbh >= 1) | (pft != 17 & dbh >= 10)))
        # pos <- as.vector(1:length(pft))
      } else {
        pos <- as.vector(1:length(pft))
      }
      
      ipa <- c(unlist(datum$cohort$ipa))[pos]
      pa_area <- c(unlist(datum$cohort$area))[pos]
      nplant <- c(unlist(datum$cohort$nplant)[pos]/pa_area)
      values <- c(unlist(datum$cohort[[cvar]]))[pos]
      pft <- pft[pos]
      dbh <- dbh[pos]
      
      if (vars[ivar] %in% c("lai","lai.change")){values <- values/nplant}
      
      Vnames <- names(values)
      years <- as.numeric(substr(Vnames,2,5))
      months <- as.numeric(substr(Vnames,7,8))
      
      temp_df <- data.frame(patch = ipa,pft,pa_area,values,nplant,years,months)
      
      cdf <- temp_df %>% group_by(pft,patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
        rename(value = S) %>% ungroup() %>% mutate(var = vars[ivar])
      
      cdf_tot <- temp_df%>% group_by(patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
        rename(value = S) %>% ungroup() %>% mutate(var = vars[ivar],
                                                   pft = 18)
      
      if (vars[ivar] %in% c("agb.change","lai.change")){
        cdf <- cdf %>% group_by(pft,patch) %>% mutate(value = value - mean(value[c(1)]))
        cdf_tot <- cdf_tot %>% group_by(patch) %>% mutate(value = value - mean(value[c(1)]))
      }
      
      df_sum2 <- bind_rows(list(df_sum2,cdf,cdf_tot))
      
    }
    
  } else {
    df_sum2 <- data.frame()
    patch_types <- unique(patches)
    
    for (patch_type in seq(1,length(patch_types))){
      cpatch <- which(patches == patch_types[patch_type])
      for (ivar in seq(1,length(vars))){
        
        cvar <- vars[ivar]
        if (cvar == "agb.change"){
          cvar = "agb"
        } else if (cvar == "lai.change"){
          cvar = "lai"
        }
        
        dbh <- unlist(datum$cohort$dbh)
        pft <- c(unlist(datum$cohort$pft))
        if(cvar %in% c("agb","npp")){
          pos <- as.vector(which((pft == 17 & dbh >= 1) | (pft != 17 & dbh >= 10)))
          # pos <- as.vector(1:length(pft))
        } else {
          pos <- as.vector(1:length(pft))
        }
        
        ipa <- c(unlist(datum$cohort$ipa))[pos]
        pa_area <- c(unlist(datum$cohort$area))[pos]
        nplant <- c(unlist(datum$cohort$nplant)[pos]/pa_area)
        values <- c(unlist(datum$cohort[[cvar]]))[pos]
        pft <- pft[pos]
        dbh <- dbh[pos]
        
        if (vars[ivar] %in% c("lai","lai.change")){values <- values/nplant}
        
        Vnames <- names(values)
        years <- as.numeric(substr(Vnames,2,5))
        months <- as.numeric(substr(Vnames,7,8))
        
        temp_df <- data.frame(patch = ipa,pft,pa_area,values,nplant,years,months)
        
        cdf <- temp_df %>% filter(patch %in% cpatch) %>% group_by(pft,patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
          rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                     var = vars[ivar])
        
        cdf_tot <- temp_df %>% filter(patch %in% cpatch) %>% group_by(patch,years,months) %>% summarise(S=sum(values*nplant)) %>%
          rename(value = S) %>% ungroup() %>% mutate(patch_t = patch_types[patch_type],
                                                     var = vars[ivar],
                                                     pft = 18)
        
        if (vars[ivar] %in% c("agb.change","lai.change")){
          cdf <- cdf %>% group_by(pft,patch) %>% mutate(value = value - mean(value[c(1)]))
          cdf_tot <- cdf_tot %>% group_by(patch) %>% mutate(value = value - mean(value[c(1)]))
        }
        
        df_sum2 <- bind_rows(list(df_sum2,cdf,cdf_tot))
        
      }
    }
  }
  
  
  return(df_sum2)
}