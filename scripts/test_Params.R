rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)

file2load <- "/home/femeunier/Documents/R/edr-da/data/All_parameters.RDS"
All.parameters <- readRDS(file2load)

Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Sanchez_PNM","Sanchez_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")
Refs_for_EDRTM <- c("Foster","Marvin","Kalacska_RTM","Sanchez")
Params_for_EDRTM <- c("b1Bl","b2Bl","orient.factor","clumping.factor","Cm")

Nsimulations <- 10
Npicks <- 50

for (isimu in seq(1,Nsimulations)){
  print(isimu/Nsimulations)
  #Pick up one reference for Prospect/ED_RTM
  Ref_prospect <- Refs_for_Prospect[sample(1:length(Refs_for_Prospect),1)]
  Ref_EDRTM <- Refs_for_EDRTM[sample(1:length(Refs_for_EDRTM),1)]
  
  params_prospect <- All.parameters %>% filter(ref == Ref_prospect,
                                               Param %in% Params_for_Prospect) %>% group_by(pft,Param) %>% sample_n(size = Npicks) %>% 
    ungroup() %>% arrange(Param) %>% mutate(id = rep(1:(Npicks),2*length(Params_for_Prospect))) %>% pivot_wider(names_from = Param,
                                                                                                                values_from = value)
  Lianas_params <- params_prospect %>% filter(pft == "Liana_optical") %>% dplyr::select(Params_for_Prospect)
  Trees_params <- params_prospect %>% filter(pft == "Tree_optical") %>% dplyr::select(Params_for_Prospect)
  
  df.spectra <- data.frame()
  for (i in seq(1,nrow(Lianas_params))){
    cparam_liana <- as.vector(t(Lianas_params[i,]))
    cparam_tree <- as.vector(t(Trees_params[i,]))
    cspectrum_liana <- prospect5(N = cparam_liana[5],cparam_liana[1],cparam_liana[2],cparam_liana[3],cparam_liana[4])
    cspectrum_tree <- prospect5(N = cparam_tree[5],cparam_tree[1],cparam_tree[2],cparam_tree[3],cparam_tree[4])
    
    df.spectra <- rbind(df.spectra,
                        rbind(data.frame(waves = 400:2500,
                                         reflectance = cspectrum_liana$reflectance,
                                         transmittance = cspectrum_liana$transmittance,
                                         pft = "Liana",
                                         simu = i),
                              data.frame(waves = 400:2500,
                                         reflectance = cspectrum_tree$reflectance,
                                         transmittance = cspectrum_tree$transmittance,
                                         pft = "Tree",
                                         simu = i)))
  }
  
  df.spectra_sum <- df.spectra %>% group_by(pft,waves) %>% summarise(r_m = mean(reflectance),
                                                                     t_m = mean(transmittance))
  
  cparams_prospect <- df.spectra_sum %>% mutate(band = case_when(waves <= 700 ~ 1,
                                                                 waves <= 2500 ~ 2)) %>% group_by(pft,band) %>% summarise(r_m = mean(r_m),
                                                                                                                          t_m = mean(t_m)) %>%
   pivot_longer(cols = c("r_m","t_m")) %>% mutate(name = case_when(name == "r_m" & band == 1 ~ "leaf_reflect_vis",
                                                                   name == "t_m" & band == 1 ~ "leaf_trans_vis",
                                                                   name == "r_m" & band == 2 ~ "leaf_reflect_nir",
                                                                   name == "t_m" & band == 2 ~ "leaf_trans_nir")) %>% dplyr::select(-c(band))
                                                  
  
  params_EDRTM <- All.parameters %>% filter(ref == Ref_EDRTM,
                                            Param %in% Params_for_EDRTM) %>% group_by(pft,Param) %>% sample_n(size = Npicks)
  
  cparams_EDRTM <- params_EDRTM %>% group_by(pft,Param) %>% summarise(value = mean(value)) %>% ungroup() %>% mutate(value = case_when(Param == "Cm" ~ 1/(10*value),
                                                                                                                     TRUE ~ value),
                                                                                                                    pft = case_when(pft == "Liana_optical" ~ "Liana",
                                                                                                                                    pft == "Tree_optical" ~ "Tree"))
  
  cparams_all <- bind_rows(list(cparams_prospect %>% rename(Param = name),
                                cparams_EDRTM)) %>% arrange(pft)

}
                         