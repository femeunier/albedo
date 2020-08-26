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

for (idir in seq_len(Ndir)){#seq_len(Ndir)
  dir_temp <- substr(all_job.file[idir],4,nchar(all_job.file[idir]))
  dir_name <- file.path(local.dir,basename(dir_temp))
  if(!dir.exists(dir_name)) dir.create(dir_name)
  
  print(dir_name)
  
  dir_out <- file.path(dirname(dirname(dir_temp)),"out",basename(dir_temp),"analy")
  
  system2("rsync",c("-avz",paste0("hpc:",file.path(dir_out,"Figures")),file.path(dir_name)))
}
