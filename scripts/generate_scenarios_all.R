rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAnRTM)
library(purrr)
library(rrtm)
library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)
library(pracma)

############################################################################
file2read <- "./smarts295_test.ext.txt"

# Irradiance (sun)
waves_all <- 400:2500 
data <- read.table(file2read, header = TRUE, sep = "", dec = ".")
colnames(data) <- c('wl','I') #Wavelength, Irradiance
data_interp <- data.frame(wl = waves_all,
                          I = interp1(data$wl,data$I,waves_all))

############################################################################

# file2load <- "~/Documents/R/edr-da/data//All_parameters.RDS"
file2load <- "/data/gent/vo/000/gvo00074/felicien/R/data/All_parameters.RDS"
All.parameters <- readRDS(file2load)

ref_dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo"
ed2in <- read_ed2in(file.path(ref_dir,"ED2IN"))

# No -T- Files
ed2in$ITOUTPUT <- 0

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

# Default config
##########################################################################################

Refs_for_Prospect <- c("Guzman","Kalacska","Castro_PNM","Castro_FTS","Sanchez_PNM","Sanchez_FTS","Foster","Marvin","Kalacska_RTM","Sanchez")
Params_for_Prospect <- c("Cab","Car","Cw","Cm","Nlayers")
Refs_for_EDRTM <- c("Foster","Marvin","Kalacska_RTM","Sanchez")
Params_for_EDRTM <- c("b1Bl","b2Bl","orient.factor","clumping.factor","Cm")

Nsimulations <- 100
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
                                         w = data_interp$I,
                                         pft = "Liana",
                                         simu = i),
                              data.frame(waves = 400:2500,
                                         reflectance = cspectrum_tree$reflectance,
                                         transmittance = cspectrum_tree$transmittance,
                                         w = data_interp$I,
                                         pft = "Tree",
                                         simu = i)))
  }
  
  df.spectra_sum <- df.spectra %>% group_by(pft,waves) %>% summarise(r_m = mean(reflectance),
                                                                     w = mean(w),
                                                                     t_m = mean(transmittance))
  
  cparams_prospect <- df.spectra_sum %>% mutate(band = case_when(waves <= 700 ~ 1,
                                                                 waves <= 2500 ~ 2)) %>% group_by(pft,band) %>% summarise(r_m = weighted.mean(r_m,w),
                                                                                                                          t_m = weighted.mean(t_m,w)) %>%
    pivot_longer(cols = c("r_m","t_m")) %>% mutate(name = case_when(name == "r_m" & band == 1 ~ "leaf_reflect_vis",
                                                                    name == "t_m" & band == 1 ~ "leaf_trans_vis",
                                                                    name == "r_m" & band == 2 ~ "leaf_reflect_nir",
                                                                    name == "t_m" & band == 2 ~ "leaf_trans_nir")) %>% dplyr::select(-c(band))
  
  
  params_EDRTM <- All.parameters %>% filter(ref == Ref_EDRTM,
                                            Param %in% Params_for_EDRTM) %>% group_by(pft,Param) %>% sample_n(size = 1)
  
  cparams_EDRTM <- params_EDRTM %>% group_by(pft,Param) %>% summarise(value = mean(value)) %>% ungroup() %>% mutate(value = case_when(Param == "Cm" ~ 1/(10*value),
                                                                                                                                      TRUE ~ value),
                                                                                                                    pft = case_when(pft == "Liana_optical" ~ "Liana",
                                                                                                                                    pft == "Tree_optical" ~ "Tree"))
  cparams_EDRTM
  cparams_all <- bind_rows(list(cparams_prospect %>% rename(Param = name),
                                cparams_EDRTM)) %>% arrange(pft)
  

  leaf_trans_vis_T <- cparams_all %>% filter(pft == "Tree",Param == "leaf_trans_vis") %>% pull(value)
  leaf_trans_nir_T <- cparams_all %>% filter(pft == "Tree",Param == "leaf_trans_nir") %>% pull(value)
  leaf_reflect_vis_T <- cparams_all %>% filter(pft == "Tree",Param == "leaf_reflect_vis") %>% pull(value)        
  leaf_reflect_nir_T <- cparams_all %>% filter(pft == "Tree",Param == "leaf_reflect_nir") %>% pull(value)
  orient_factor_T <- cparams_all %>% filter(pft == "Tree",Param == "orient.factor") %>% pull(value)
  clumping_factor_T <- cparams_all %>% filter(pft == "Tree",Param == "clumping.factor") %>% pull(value)
  
  leaf_trans_vis_L <- cparams_all %>% filter(pft == "Liana",Param == "leaf_trans_vis") %>% pull(value)
  leaf_trans_nir_L <- cparams_all %>% filter(pft == "Liana",Param == "leaf_trans_nir") %>% pull(value)
  leaf_reflect_vis_L <- cparams_all %>% filter(pft == "Liana",Param == "leaf_reflect_vis") %>% pull(value)      
  leaf_reflect_nir_L <- cparams_all %>% filter(pft == "Liana",Param == "leaf_reflect_nir") %>% pull(value)
  orient_factor_L <- cparams_all %>% filter(pft == "Liana",Param == "orient.factor") %>% pull(value)
  clumping_factor_L <- cparams_all %>% filter(pft == "Liana",Param == "clumping.factor") %>% pull(value) 
  
  #########################################################################################
  
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
  
  run_name <- paste0("ref_final",isimu)
  
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
  
  #######################################################################################
  run_name <-  paste0("liana_final_",isimu)
  
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
  config_default$Liana["orient_factor"] <- orient_factor_L
  config_default$Liana["clumping_factor"] <- clumping_factor_L
  config_default$Liana["leaf_reflect_vis"] <- leaf_reflect_vis_L
  config_default$Liana["leaf_reflect_nir"] <- leaf_reflect_nir_L
  config_default$Liana["leaf_trans_vis"]   <- leaf_trans_vis_L
  config_default$Liana["leaf_trans_nir"]   <- leaf_trans_nir_L
  
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
  
  
  
  #######################################################################################
  
  dumb <- write_bash_submission(file = file.path(rundir,"all_jobs.sh"),
                                list_files = list_dir,
                                job_name = "job.sh")
  
}
