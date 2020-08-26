#' Write PEcAn standard output files
#'
#' @export
#' 
model2ncdf_datum <- function(datum,sitelat=-9.15,sitelon=-80,start_date="2004/01/01",outdir, N = 48){
  
  start_year <- lubridate::year(start_date)
  
  # create lat/long/lambda nc dimensions
  lat <- ncdf4::ncdim_def("lat", "degrees_north",
                          vals = as.numeric(sitelat),
                          longname = "station_latitude")
  
  lon <- ncdf4::ncdim_def("lon", "degrees_east",
                          vals = as.numeric(sitelon),
                          longname = "station_longitude")
  
  out <- list(GPP = median(datum$emean$gpp[1:N]),
              GPP_liana =  median(datum$szpft$gpp[1:N,12,17]),
              GPP_tree = median(apply(datum$szpft$gpp[1:N,12,c(2,3,4)],1,sum)),
              NPP = median(datum$emean$npp[1:N]),
              NPP_liana =  median(datum$szpft$npp[1:N,12,17]),
              NPP_tree = median(apply(datum$szpft$npp[1:N,12,c(2,3,4)],1,sum)),
              albedo_par = median(datum$emean$parup[1:N]),
              albedo_nir = median(datum$emean$rshortup[1:N] - datum$emean$parup[1:N]/4.6),
              leaf.par = median(datum$emean$leaf.par),
              leaf.par_liana =  median(datum$szpft$leaf.par[1:N,12,17]),
              leaf.par_tree = median(apply(datum$szpft$leaf.par[1:N,12,c(2,3,4)],1,sum)),
              nep = median(datum$emean$nep[1:N]),
              par.ground = median(datum$emean$par.gnd[1:N]),
              nir.ground = median(datum$emean$rshort.gnd[1:N] - datum$emean$par.gnd[1:N]/4.6),
              rshort.ground = median(datum$emean$rshort.gnd[1:N]),
              nplant = median(datum$emean$nplant[1:N]),
              nplant_liana = median(datum$szpft$nplant[1:N,12,17]),
              nplant_tree = median(apply(datum$szpft$nplant[1:N,12,c(2,3,4)],1,sum)))
              
  
  
  # Create netcdf files
  nc_var <- list()
  nc_var[[1]] <- ncdf4::ncvar_def("GPP", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Ecosystem productivity")
  
  nc_var[[2]] <- ncdf4::ncvar_def("GPP_liana", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[3]] <- ncdf4::ncvar_def("GPP_tree", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[4]] <- ncdf4::ncvar_def("NPP", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Ecosystem productivity")
  
  nc_var[[5]] <- ncdf4::ncvar_def("NPP_liana", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[6]] <- ncdf4::ncvar_def("NPP_tree", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[7]] <- ncdf4::ncvar_def("albedo_par", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Albedo PAR")
  
  nc_var[[8]] <- ncdf4::ncvar_def("albedo_nir", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Albedo NIR")
  
  nc_var[[9]] <- ncdf4::ncvar_def("leaf.par", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[10]] <- ncdf4::ncvar_def("leaf.par_liana", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Liana productivity")
  
  nc_var[[11]] <- ncdf4::ncvar_def("leaf.par_tree", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Albedo PAR")
  
  nc_var[[12]] <- ncdf4::ncvar_def("nep", units = "-", dim = list(lon, lat), missval = -999, 
                                  longname = "Albedo NIR")
  
  nc_var[[13]] <- ncdf4::ncvar_def("par.gnd", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Albedo PAR")
  
  nc_var[[14]] <- ncdf4::ncvar_def("nir.gnd", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Albedo NIR")
  
  nc_var[[15]] <- ncdf4::ncvar_def("rshort.gnd", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Albedo NIR")
  
  nc_var[[16]] <- ncdf4::ncvar_def("nplant", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Liana productivity")
  
  nc_var[[17]] <- ncdf4::ncvar_def("nplant_tree", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Albedo PAR")
  
  nc_var[[18]] <- ncdf4::ncvar_def("nplant_liana", units = "-", dim = list(lon, lat), missval = -999, 
                                   longname = "Liana productivity")
  
  
  nc <- ncdf4::nc_create(file.path(outdir, paste(start_year, "nc", sep = ".")), nc_var)
  
  for (i in seq(nc_var)){
    ncdf4::ncvar_put(nc, nc_var[[i]],out[[i]])
  }
  ncdf4::nc_close(nc)
}  