rm(list = ls())

library(stringr)

local.dir <- "~/Documents/projects/albedo/outputs/"
if(!dir.exists(local.dir)) dir.create(local.dir)

remote.dir <- "/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo/run"

file <- file.path(remote.dir,"all_jobs.sh")
system2("scp",c(paste0("hpc:",file),local.dir))

local.file <- file.path(local.dir,"all_jobs.sh")
all_job.file <- readLines(local.file)[seq(2,length(readLines(local.file)),2)]

Ndir <- length(all_job.file)

# for (idir in seq_len(Ndir)){#seq_len(Ndir)
scenarios <- c("ref_v2_","liana_all_v2_")
simulations <- 100

for (iscenar in seq(1,length(scenarios))){
  for (isimu in seq(1,simulations)){
    # dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
    dir_temp <- file.path(dirname(remote.dir),"out",paste0(scenarios[iscenar],isimu))
    dir_name <- file.path(local.dir,basename(dir_temp))
    if(!dir.exists(dir_name)) dir.create(dir_name)
    
    print(dir_name)
    
    dir_out <- file.path(dirname(dirname(dir_temp)),"out",basename(dir_temp),"analy")
    dir_in <- file.path(dirname(dirname(dir_temp)),"run",basename(dir_temp))
    
    system2("rsync",c("-avz --no-recursive",paste0("hpc:",file.path(dir_out,"*.RData")),file.path(dir_name)))
    system2("rsync",c("-avz --no-recursive",paste0("hpc:",file.path(dir_in,"*.xml")),file.path(dir_name)))
  }
}


# sudo rsync -zarv  --include "*/" --exclude="*" --include="*.RData" hpc:/data/gent/vo/000/gvo00074/pecan/output/other_runs/albedo/out/ outputs/


