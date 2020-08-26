rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(ggplot2)
library(ggridges)
library(cowplot)
library(pracma)
library(albedo)
library(PEcAn.uncertainty)
library(PEcAn.settings)
library(redr)

#####################################################################################################################
# 1) Generate samplings

file2read <- "~/Documents/R/edr-da/data/smarts295_test.ext.txt"
# Irradiance (sun)
waves_all <- 400:2500 
data <- read.table(file2read, header = TRUE, sep = "", dec = ".")
colnames(data) <- c('wl','I') #Wavelength, Irradiance
data_interp <- data.frame(wl = waves_all,
                          I = interp1(data$wl,data$I,waves_all))

file2load <- "~/Documents/R/edr-da/data/All_parameters.RDS"

All.parameters <- readRDS(file2load)

Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")

All.parameters <- All.parameters %>% filter(ref %in% Refs_for_Prospect)
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")

Nsimulations <- 500
Npicks <- 1

df.spectra.all <- spectra.all <- df.diff.all <- diff.all <- data.frame()

for (isimu in seq(1,Nsimulations)){
  
  print(isimu/Nsimulations)
  Ref_prospect <- Refs_for_Prospect[sample(1:length(Refs_for_Prospect),1)]
  
  params_prospect <- All.parameters %>% filter(Param %in% Params_for_Prospect) %>% group_by(pft,Param) %>% sample_n(size = Npicks) %>%
    ungroup() %>% arrange(Param) %>% mutate(id = rep(1:Npicks,2*length(Params_for_Prospect))) %>% dplyr::select(-ref) %>% pivot_wider(names_from = Param,
                                                                                                                                      values_from = value)
  
  Lianas_params <- params_prospect %>% filter(pft == "Liana_optical") %>% dplyr::select(Params_for_Prospect)
  Trees_params <- params_prospect %>% filter(pft == "Tree_optical") %>% dplyr::select(Params_for_Prospect)
  
  df.spectra <- data.frame()
  
  for (i in seq(1,Npicks)){
    cparam_liana <- as.vector(t(Lianas_params[i,]))
    cparam_tree <- as.vector(t(Trees_params[i,]))
    cspectrum_liana <- prospect5(N = cparam_liana[5],cparam_liana[1],cparam_liana[2],cparam_liana[3],cparam_liana[4])
    cspectrum_tree <- prospect5(N = cparam_tree[5],cparam_tree[1],cparam_tree[2],cparam_tree[3],cparam_tree[4])
    
    cspectra <- rbind(data.frame(waves = 400:2500,
                                 reflectance = cspectrum_liana$reflectance,
                                 transmittance = cspectrum_liana$transmittance,
                                 w = data_interp$I,
                                 pft = "Liana",
                                 rep = i),
                      data.frame(waves = 400:2500,
                                 reflectance = cspectrum_tree$reflectance,
                                 transmittance = cspectrum_tree$transmittance,
                                 w = data_interp$I,
                                 pft = "Tree",
                                 rep = i))
    df.spectra <- bind_rows(list(df.spectra,
                                 cspectra %>% mutate(Band = case_when(waves >= 500 & waves <= 650 ~ 1,
                                                                      waves <= 2500 ~2)) %>% filter(Band %in%c(1,2)) %>% group_by(Band,pft) %>%
                                   summarise(reflectance = weighted.mean(reflectance,w),
                                             transmittance = weighted.mean(transmittance,w)) %>% mutate(rep = i)))
    
  }
  
  
  df.spectra.all <- bind_rows(list(df.spectra.all,
                                   df.spectra %>% mutate(simu = isimu)))
  
}

Param_EDRTM <- rbind(df.spectra.all %>% filter(Band == 1) %>% select(pft,reflectance) %>% mutate(Param = "leaf_reflect_vis") %>% rename(value = reflectance),
                     df.spectra.all %>% filter(Band == 2) %>% select(pft,reflectance) %>% mutate(Param = "leaf_reflect_nir") %>% rename(value = reflectance),
                     df.spectra.all %>% filter(Band == 1) %>% select(pft,transmittance) %>% mutate(Param = "leaf_trans_vis") %>% rename(value = transmittance),
                     df.spectra.all %>% filter(Band == 2) %>% select(pft,transmittance) %>% mutate(Param = "leaf_trans_nir") %>% rename(value = transmittance))

Refs_for_EDRTM <- c("b1Bl","b2Bl","orient.factor","clumping.factor")
Param_others <-
  All.parameters %>% filter(Param %in% Refs_for_EDRTM)%>% mutate(Param = as.character(Param),
                       Param = case_when(
                         Param == "orient.factor" ~ "orient_factor",
                         Param == "clumping.factor" ~ "clumping_factor",
                         TRUE ~ Param
                       )) %>% dplyr::select(-ref)

All_samplings <- bind_rows(list(Param_EDRTM,Param_others)) %>% mutate(pft = case_when(pft == "Tree_optical" ~ "Tree",
                                                                                      pft == "Liana_optical" ~ "Liana",
                                                                                      TRUE ~ pft))


#####################################################################################################################
# 2) Model2ncdf

All_pfts <- c("Tree_optical","Liana_optical")
All_pfts_names <- c("Tree","Liana")
All_params <- c('b1Bl',"b2Bl","clumping_factor","leaf_reflect_nir","leaf_reflect_vis","leaf_trans_nir","leaf_trans_vis","orient_factor")
All_quantiles_names <- c(0.159,0.841) 

maindir <- "/home/femeunier/Documents/projects/albedo/outputs/"
load(file.path(maindir,"SA-median","analysis.RData"))
# datum_ref <- datum
# 
# maindir <- "/home/femeunier/Documents/projects/albedo/"
# load(file.path(maindir,"outputs/SA-Liana-leaf_reflect_vis-0.841/","analysis.RData"))
# load(file.path(maindir,"outputs/SA-Liana-clumping_factor-0.159//","analysis.RData"))
# datum_mod <- datum
# 
# plot((datum_ref$szpft$recr[,12,18] - datum_mod$szpft$recr[,12,18]),type='l')

N = 12
model2ncdf_datum(datum,sitelat=-9.15,sitelon=-80,start_date="2004/01/01",outdir = file.path(maindir,"SA-median"), N = N)

for (iparam in seq(1,length(All_params))){
  for (ipft in seq(1,length(All_pfts))){
    for (iquantile in seq(1,length(All_quantiles_names))){
      simu_name <- file.path(maindir,paste0("SA-",All_pfts_names[ipft],"-",All_params[iparam],"-",All_quantiles_names[iquantile]))
      load(file.path(simu_name,"analysis.RData"))
      model2ncdf_datum(datum,sitelat=-9.15,sitelon=-80,start_date="2004/01/01",outdir = simu_name, N = N)
    }
  }
}



#####################################################################################################################
# 3) All variables to save

All_quantiles_names <- c(0.159,0.5,0.841) 
Nparams <- length(All_params) 
pecanxmlfiles <- c("./pecan.CONFIGS_tree.xml")

pecanxmlfile <- pecanxmlfiles
settings <- PEcAn.settings::read.settings(pecanxmlfile)

sa.run.ids <- list()
sa.samples <- list(env = data.frame())
trait.names <- trait.samples <- list()

for (ipft in seq(1,length(All_pfts_names))){
  
  mat <- runs <- array(NA,c(length(All_quantiles_names),Nparams))
  rownames(mat) <-  rownames(runs) <- All_quantiles_names*100
  colnames(mat) <- colnames(runs) <- All_params
  
  for (iparam in seq(1,Nparams)){
    for (iquantile in seq(All_quantiles_names)){
      simname <- paste0("SA-",All_pfts_names[ipft],"-",All_params[iparam],"-",All_quantiles_names[iquantile])
      
      sampling <- All_samplings %>% filter(pft == All_pfts_names[ipft] & Param == All_params[iparam]) %>% pull(value)
      
      mat[iquantile,iparam] <- quantile(sampling,All_quantiles_names[iquantile])
      if (All_quantiles_names[iquantile] != 0.5){
        runs[iquantile,iparam] <- simname
      } else {
        runs[iquantile,iparam] <- "SA-median"       
      }
    }
    trait.samples[[All_pfts[ipft]]][[All_params[iparam]]] <- sampling
  }
  
  sa.run.ids[[All_pfts[ipft]]] <- runs
  sa.samples[[All_pfts[ipft]]] <- as.data.frame(mat)
  trait.names[[All_pfts[ipft]]] <- All_params

}

save(sa.run.ids,All_pfts,trait.names,trait.samples,sa.samples, file = file.path(settings$outdir,"sensitivity.samples.NOENSEMBLEID.Rdata"))
save(trait.names,All_pfts,trait.samples, file = file.path(settings$outdir,"samples.Rdata"))

vars <- c("GPP","rshort.gnd")
# vars <- c('leaf.par_liana',"leaf.par_tree",'leaf.par')
# vars <- c("par.gnd","nir.gnd","rshort.gnd")

mapdf <- data.frame(pfts=All_pfts,col=c("blue","green"))

model_sensitivities_all <- data.frame()

for (ivar in seq(1,length(vars))){
  variable = vars[ivar]
  OP_all <- VDP_allPFTs(variable = variable,mapdf = mapdf)

  model_sensitivities_all <-
    bind_rows(list(model_sensitivities_all,
          data.frame(OP_variable = vars[ivar],
                     pft = OP_all$PFT_all,
                     CV=OP_all$coef.vars,
                     sens = OP_all$sensitivities,
                     elas = OP_all$elasticities,
                     variance = OP_all$variances,
                     par.var = OP_all$partial.variances,
                     variable = names(OP_all$elasticities))))
  
}

model_sensitivities_all <- as.data.frame(model_sensitivities_all)
# model_sensitivities_all %>% filter(par.var > 0.05)

model_sensitivities_format <-
  model_sensitivities_all %>% mutate(
    OP_variable = case_when(
      OP_variable == "NPP_liana" ~ "Liana NPP",
      OP_variable == "NPP_tree"  ~ "Tree NPP",
      OP_variable == "NPP" ~ "Ecosystem NPP",
      OP_variable == "GPP" ~ "Ecosystem GPP",
      OP_variable == "rshort.gnd" ~ "Understorey \n shortwave light",
      OP_variable == "albedo_par" ~ "Albedo PAR \n [400-700nm]",
      OP_variable == "albedo_nir" ~ "Albedo IR \n [701-2500nm]",
      OP_variable == "nep" ~ "NEP",
      OP_variable == "par.gnd" ~ "Transmitted PAR",
      OP_variable == "nir.gnd" ~ "Transmitted IR",
      OP_variable == "leaf.par_liana" ~ "PAR intercepted by lianas",
      TRUE ~ OP_variable
    )
  ) %>% mutate(variable = as.character(variable),
    variable = case_when(
      variable == "orient_factor" ~ "ω",
      variable == "clumping_factor" ~ "Ω",
      variable == "leaf_reflect_vis" ~ "ρ[vis]",
      variable == "leaf_trans_vis" ~ "τ[vis]",
      variable == "leaf_reflect_nir" ~ "ρ[IR]",
      variable == "leaf_trans_nir" ~ "τ[IR]",
      TRUE ~ variable
    )
  ) %>% mutate(variable = as.factor(variable),
               OP_variable = as.factor(OP_variable))

levels(model_sensitivities_format$OP_variable) <- levels(model_sensitivities_format$OP_variable)[c(2,1,3,4)]

ggplot(data = model_sensitivities_format) +
  geom_bar(aes(x=variable, y = (par.var*100),fill = pft),stat = "identity",position = "dodge",alpha = 0.5) +
  coord_flip() +
  facet_wrap(OP_variable ~ .) +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  labs(x = "",y = "Partial variance [%]",fill = "") +
  scale_x_discrete(labels = parse(text = levels(model_sensitivities_format$variable))) +
  theme_bw() + theme(text = element_text(size = 20)) +
  guides(fill = FALSE)
# 
# A <- 
#   ggplot(data = model_sensitivities_format %>% filter(OP_variable %in%  levels(model_sensitivities_format$OP_variable)[c(1,2)])) +
#   geom_bar(aes(x=variable, y = par.var*100,fill = pft),stat = "identity",position = "dodge",alpha = 0.5) +
#   coord_flip() +
#   facet_grid(OP_variable ~ .) +
#   scale_fill_manual(values = c("#1E64C8","#137300")) +
#   labs(x = "",y = "Partial variance [%]",fill = "") +
#   scale_x_discrete(labels = parse(text = levels(model_sensitivities_format$variable))) +
#   theme_bw() + theme(text = element_text(size = 20)) +
#   guides(fill = FALSE)
# 
# B <- 
#   ggplot(data = model_sensitivities_format %>% filter(OP_variable %in%  levels(model_sensitivities_format$OP_variable)[c(3,4)])) +
#   geom_bar(aes(x=variable, y = par.var*100,fill = pft),stat = "identity",position = "dodge",alpha = 0.5) +
#   coord_flip() +
#   facet_grid(OP_variable ~ .) +
#   scale_fill_manual(values = c("#1E64C8","#137300")) +
#   labs(x = "",y = "Partial variance [%]",fill = "") +
#   scale_x_discrete(labels = parse(text = levels(model_sensitivities_format$variable))) +
#   theme_bw() + theme(text = element_text(size = 20)) +
#   guides(fill = FALSE)
# 
# plot_grid(A,B,nrow = 1, align = "hv")

ggsave(plot = last_plot(),
       dpi = 300,
       width = 30,
       height = 20,
       units = "cm",
       file = "./Figures/SA_ED2.png")
