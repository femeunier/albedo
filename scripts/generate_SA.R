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


#####################################################################################################################
file2read <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/smarts295_test.ext.txt"
file2read <- "~/Documents/R/edr-da/data/smarts295_test.ext.txt"
# Irradiance (sun)
waves_all <- 400:2500 
data <- read.table(file2read, header = TRUE, sep = "", dec = ".")
colnames(data) <- c('wl','I') #Wavelength, Irradiance
data_interp <- data.frame(wl = waves_all,
                          I = interp1(data$wl,data$I,waves_all))

######################################################################################################################


file2load <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/edr-da/data/All_parameters.RDS"
file2load <- "~/Documents/R/edr-da/data/All_parameters.RDS"

All.parameters <- readRDS(file2load)

Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")

All.parameters <- All.parameters %>% filter(ref %in% Refs_for_Prospect)
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")

Nsimulations <- 100
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


CI <- df.spectra.all %>% group_by(Band,pft) %>% summarise(r_m = mean(reflectance),
                                                          r_sd = sd(reflectance),
                                                          t_m = mean(transmittance),
                                                          t_sd = sd(transmittance)) %>% ungroup() 

CI1 <- rbind(CI %>% filter(Band == 1) %>% select(pft,r_m,r_sd) %>% mutate(Param = "leaf_reflect_vis") %>% rename (value_m = r_m,value_sd = r_sd),
             CI %>% filter(Band == 2) %>% select(pft,r_m,r_sd) %>% mutate(Param = "leaf_reflect_nir") %>% rename (value_m = r_m,value_sd = r_sd),
             CI %>% filter(Band == 1) %>% select(pft,t_m,t_sd) %>% mutate(Param = "leaf_trans_vis") %>% rename (value_m = t_m,value_sd = t_sd),
             CI %>% filter(Band == 2) %>% select(pft,t_m,t_sd) %>% mutate(Param = "leaf_trans_nir") %>% rename (value_m = t_m,value_sd = t_sd))

Refs_for_EDRTM <- c("b1Bl","b2Bl","orient.factor","clumping.factor")
CI_others <-
  All.parameters %>% filter(Param %in% Refs_for_EDRTM) %>% group_by(Param, pft) %>% summarise(value_m = mean(value),
                                                                                              value_sd = sd(value)) %>% mutate(pft = case_when(pft == "Liana_optical" ~ "Liana",
                                                                                                                                               pft == "Tree_optical" ~ "Tree")) %>%
  ungroup() %>% mutate(Param = as.character(Param),
    Param = case_when(
      Param == "orient.factor" ~ "orient_factor",
      Param == "clumping.factor" ~ "clumping_factor",
      TRUE ~ Param
    )
  )

CI_all <- bind_rows(list(CI1,CI_others))  


###############################################################################################################

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))

# No -T- Files
ed2in$ITOUTPUT <- 3

rundir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo/run"
outdir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo/out"

if(!dir.exists(rundir)) dir.create(rundir)
if(!dir.exists(outdir)) dir.create(outdir)

##############################################################################
# Default

PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"
defaults <- list_dir <- list()

# Default settings
settings <- list(model = list(revision = "git",
                              config.header = NULL),
                 pfts = list(pft = list(num = 2,
                                        ed2_pft_number = 2,
                                        name = "Early"),
                             pft = list(num = 3,
                                        ed2_pft_number = 3,
                                        name = "Mid"),
                             pft = list(num = 4,
                                        ed2_pft_number = 4,
                                        name = "Late"),
                             pft = list(num = 17,
                                        ed2_pft_number = 17,
                                        name = "Liana")))

leaf_trans_vis_T <- CI_all %>% filter(pft == "Tree",Param == "leaf_trans_vis") %>% pull(value_m)
leaf_trans_nir_T <- CI_all %>% filter(pft == "Tree",Param == "leaf_trans_nir") %>% pull(value_m)
leaf_reflect_vis_T <- CI_all %>% filter(pft == "Tree",Param == "leaf_reflect_vis") %>% pull(value_m)          
leaf_reflect_nir_T <-CI_all %>% filter(pft == "Tree",Param == "leaf_reflect_nir") %>% pull(value_m)
orient_factor_T <- CI_all %>% filter(pft == "Tree",Param == "orient.factor") %>% pull(value_m)
clumping_factor_T <- CI_all %>% filter(pft == "Tree",Param == "clumping.factor") %>% pull(value_m)

leaf_trans_vis_L <- CI_all %>% filter(pft == "Liana",Param == "leaf_trans_vis") %>% pull(value_m)
leaf_trans_nir_L <- CI_all %>% filter(pft == "Liana",Param == "leaf_trans_nir") %>% pull(value_m)
leaf_reflect_vis_L <- CI_all %>% filter(pft == "Liana",Param == "leaf_reflect_vis") %>% pull(value_m)          
leaf_reflect_nir_L <-CI_all %>% filter(pft == "Liana",Param == "leaf_reflect_nir") %>% pull(value_m)
orient_factor_L <- CI_all %>% filter(pft == "Liana",Param == "orient.factor") %>% pull(value_m)
clumping_factor_L <- CI_all %>% filter(pft == "Liana",Param == "clumping.factor") %>% pull(value_m) 

# Default config
config <- list()
config[["Early"]] <- unlist(list(clumping_factor = clumping_factor_T,
                                 seedling_mortality = 0.98,
                                 Vcmax = 16*2.4,
                                 Vm0 = 16*2.4,
                                 leaf_trans_vis = leaf_trans_vis_T,
                                 leaf_trans_nir = leaf_trans_nir_T,
                                 leaf_reflect_vis = leaf_reflect_vis_T,
                                 leaf_reflect_nir = leaf_reflect_nir_T,
                                 orient_factor = orient_factor_T))
config[["Mid"]] <- unlist(list(clumping_factor = clumping_factor_T,
                               Vcmax = 13*2.4,
                               Vm0 = 13*2.4,
                               leaf_trans_vis = leaf_trans_vis_T,
                               leaf_trans_nir = leaf_trans_nir_T,
                               leaf_reflect_vis = leaf_reflect_vis_T,
                               leaf_reflect_nir = leaf_reflect_nir_T,
                               orient_factor = orient_factor_T))
config[["Late"]] <- unlist(list(clumping_factor = clumping_factor_T,
                                Vcmax = 4.5*2.4,
                                Vm0 = 4.5*2.4,
                                wood_Kmax = 0.008,
                                leaf_trans_vis = leaf_trans_vis_T,
                                leaf_trans_nir = leaf_trans_nir_T,
                                leaf_reflect_vis = leaf_reflect_vis_T,
                                leaf_reflect_nir = leaf_reflect_nir_T,
                                orient_factor = orient_factor_T))
config[["Liana"]] <- unlist(
  list(
    rho = 0.462893312003502,
    wood_Kexp = 2.06151664261015,
    Vcmax = 21.0195095978388 * 2.4,
    wood_Kmax = 0.118592088619329,
    wood_water_cap = 0.00831146542859373*1000,
    b1Rd = 0.251705611238744,
    b2Rd = 0.251058588541278,
    wood_psi50 = 122.88209151827,
    growth_resp_factor = 0.352803405024027,
    SLA = 22.9831799052029 * 0.48,
    b1Bl_large = 0.0957164598030354,
    stoma_psi_b = 160.017481634853,
    root_respiration_factor = 0.280639319284819,
    b2Ht = 0.868131191794218,
    SRA = 48.1711743548512,
    r_fract = 0.826262914185645,
    stomatal_slope = 10.4797428731951,
    root_beta = 0.0501418540509767,
    b2Bl_large = 1.84721490377007,
    b1Bs_large = 0.271899528000708,
    b2Bs_large = 2.57118662996341,
    b1Ht = 0.100034825515468,
    q = 0.994400362018496,
    mort2 = 15.3333587065344,
    leaf_turnover_rate = 1.85273895977298,
    root_turnover_rate = 1.27805201890461,
    stoma_psi_c = 2.9926889645867,
    dark_respiration_factor = 0.0279573623213031,
    quantum_efficiency = 0.057162389334215,
    mort3 = 0.0508703883618926,
    leaf_psi_tlp = 204.690265902307,
    leaf_water_cap = 0.00189950774801228*100,
    Vm0 = 21.0195095978388 * 2.4,
    clumping_factor = clumping_factor_T,
    seedling_mortality = 0.98,
    leaf_trans_vis = leaf_trans_vis_T,
    leaf_trans_nir = leaf_trans_nir_T,
    leaf_reflect_vis = leaf_reflect_vis_T,
    leaf_reflect_nir = leaf_reflect_nir_T,
    orient_factor = orient_factor_T))



##########################################################################################
# Reference simulation

run_name <- "SA-median"
  
run_ref <- file.path(rundir,run_name)
out_ref <- file.path(outdir,run_name)

if(!dir.exists(run_ref)) dir.create(run_ref)
if(!dir.exists(out_ref)) dir.create(out_ref)
if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))

# ED2IN
ed2in_scenar <- ed2in
ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")

write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

# Config
config_default <- config
xml <- write.config.xml.ED2(defaults = defaults,
                            settings = settings,
                            trait.values = config_default)

XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
             prefix = PREFIX_XML)

# job.sh
write_job(file =  file.path(run_ref,"job.sh"),
          nodes = 1,ppn = 18,mem = 16,walltime = 4,
          prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
          CD = run_ref,
          ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/run/ed_2.1-opt",
          ED2IN = "ED2IN")

list_dir[[run_name]] = run_ref


##################################################################################################################"
# Quantiles simulations

All_pfts <- unique(CI_all$pft)
All_params <- unique(CI_all$Param)
All_quantiles <- c(-1,1)
All_quantiles_names <- c(0.159,0.841) 
for (iparam in seq(1,length(All_params))){
  for (ipft in seq(1,length(All_pfts))){
    for (iquantile in seq(1,length(All_quantiles))){
      param2change <- CI_all %>% filter(pft == All_pfts[ipft],
                                        Param == All_params[iparam])
      if (All_params[iparam] == "b1Bl") {param2change = "b1Bl_large"}
      if (All_params[iparam] == "b2Bl") {param2change = "b2Bl_large"}
      
      
      paramvalue <- param2change %>% pull(value_m) + All_quantiles[iquantile]*param2change %>% pull(value_sd)
      
      simu_name <- paste0("SA-",All_pfts[ipft],"-",All_params[iparam],"-",All_quantiles_names[iquantile])
      
      #######################################################################################
      run_name <-  simu_name
      
      run_ref <- file.path(rundir,run_name)
      out_ref <- file.path(outdir,run_name)
      
      if(!dir.exists(run_ref)) dir.create(run_ref)
      if(!dir.exists(out_ref)) dir.create(out_ref)
      if(!dir.exists(file.path(out_ref,"analy"))) dir.create(file.path(out_ref,"analy"))
      if(!dir.exists(file.path(out_ref,"histo"))) dir.create(file.path(out_ref,"histo"))
      
      # ED2IN
      ed2in_scenar <- ed2in
      ed2in_scenar$IEDCNFGF <- file.path(run_ref,"config.xml")
      ed2in_scenar$FFILOUT = file.path(out_ref,"analy","analysis")
      ed2in_scenar$SFILOUT = file.path(out_ref,"histo","history")
      
      write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))
      
      # Config
      config_default <- config
      
      if (ipft == 1){
        config_default[["Liana"]][param2change] <- paramvalue
      } else{
        config_default[["Early"]][param2change] <- paramvalue
        config_default[["Mid"]][param2change] <- paramvalue
        config_default[["Late"]][param2change] <- paramvalue
      }


      
      xml <- write.config.xml.ED2(defaults = defaults,
                                  settings = settings,
                                  trait.values = config_default)
      
      XML::saveXML(xml, file = file.path(run_ref,"config.xml"), indent = TRUE,
                   prefix = PREFIX_XML)
      
      # job.sh
      write_job(file =  file.path(run_ref,"job.sh"),
                nodes = 1,ppn = 18,mem = 16,walltime = 4,
                prerun = "ml UDUNITS/2.2.26-intel-2018a R/3.4.4-intel-2018a-X11-20180131 HDF5/1.10.1-intel-2018a; ulimit -s unlimited",
                CD = run_ref,
                ed_exec = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2/ED/run/ed_2.1-opt",
                ED2IN = "ED2IN")
      
      list_dir[[run_name]] = run_ref
      
      
      
      
    }
  }
}

dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                              list_files = list_dir,
                              job_name = "job.sh")

