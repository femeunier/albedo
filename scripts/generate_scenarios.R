rm(list = ls())

library(ED2scenarios)
library(PEcAn.ED2)
library(purrr)

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
              
leaf_trans_vis_T <- 0.01 
leaf_trans_nir_T <- 0.22
leaf_reflect_vis_T <- 0.0516            
leaf_reflect_nir_T <- 0.299
orient_factor_T <- -0.1
clumping_factor_T <- 0.6
  
leaf_trans_vis_L <- 0.0244
leaf_trans_nir_L <- 0.244
leaf_reflect_vis_L <- 0.0665            
leaf_reflect_nir_L <- 0.300
orient_factor_L <- 0.4
clumping_factor_L <- 0.7   
  
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

run_name <- "ref"

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

##########################################################################################
# Reference simulation no liana

run_name <- "no_liana"

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
ed2in_scenar$INCLUDE_THESE_PFT = c(2,3,4)

write_ed2in(ed2in_scenar,filename = file.path(run_ref,"ED2IN"))

# Config
config_default <- config
config_default[["Liana"]] <- NULL
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

run_name <- "liana_RTM"

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

run_name <- "liana_other"

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

run_name <- "liana_all"

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

