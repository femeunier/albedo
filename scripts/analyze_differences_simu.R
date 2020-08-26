rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(albedo)

directory <- "/home/femeunier/Documents/projects/albedo/outputs/"
scenarios <- c("ref","liana_all")
scenarios <- c("ref_v2_","liana_all_v2_")
scenarios <- c("ref_final","liana_final_")

simulations <- 100

df <- data.frame()
df_params <- data.frame()

filter.out <- c()
for (isimu in seq(1,simulations)){
  print(isimu/simulations)
  for (iscenar in seq(1,length(scenarios))){
    
    file.datum <- file.path(directory,paste0(scenarios[iscenar],isimu),"analysis.RData")
    
    if (file.exists(file.datum)){
        load(file.datum)
  
      weights <- diff(c(datum$slz,0))
      weights[datum$slz < -0.21] <- 0
      weights <- weights/sum(weights)
      
      # If I want to test liana-infested vs liana-free patches
      # LAI <- datum$cohort$lai
      # H <- datum$cohort$height
      # PFT <- datum$cohort$pft
      # IPA <- datum$cohort$ipa
      # AREA <- datum$patch$area
      # 
      # i = 13
      # 
      # LAI2 <- LAI
      # LAI2[[i]][PFT[[i]] != 17] = 0
      # LAI_liana <- aggregate(x = LAI2[[i]],by = list(ipa = IPA[[i]]),FUN = sum)[["x"]]
      # LAI_tot <- aggregate(x = LAI[[i]],by = list(ipa = IPA[[i]]),FUN = sum)[["x"]]
      # plot(LAI_liana[LAI_tot>3],datum$patch$rshortup[[i]][LAI_tot>3]-datum$patch$parup[[i]][LAI_tot>3]/4.6)
      # 
      # plot(LAI_liana[LAI_tot>3],datum$patch$parup[[i]][LAI_tot>3],ylim = c(10,12))
      # cat <- rbind(data.frame(LAIliana = 0,parup = datum$patch$parup[[i]][LAI_tot>3 & LAI_liana<2]),
      #              data.frame(LAIliana = 1,parup = datum$patch$parup[[i]][LAI_tot>3 & LAI_liana>2]))
      # ggplot(cat) +
      #   geom_boxplot(aes(x = as.factor(LAIliana),y = parup)) +
      #   theme_bw()
      
      df <- bind_rows(list(df,
                  data.frame(nep = datum$emean$npp - datum$emean$het.resp,
                             LAI = datum$emean$lai,
                             LAI_liana = datum$szpft$lai[,12,c(17)],
                             LAI_tree = apply(datum$szpft$lai[,12,c(2,3,4)],1,sum),
                             AGB = datum$emean$agb,
                             AGB_liana = datum$szpft$agb[,12,c(17)],
                             AGB_tree = apply(datum$szpft$agb[,12,c(2,3,4)],1,sum),
                             Rauto = datum$emean$plant.resp,
                             Rauto_liana = datum$szpft$plant.resp[,12,c(17)],
                             Rauto_tree = apply(datum$szpft$plant.resp[,12,c(2,3,4)],1,sum),
                             Rhetero = datum$emean$het.resp,
                             Reco = datum$emean$reco,
                             gpp = datum$emean$gpp,
                             gpp_tree = apply(datum$szpft$gpp[,12,c(2,3,4)],1,sum),
                             gpp_liana = datum$szpft$gpp[,12,c(17)],
                             npp = datum$emean$npp,
                             npp_tree = apply(datum$szpft$npp[,12,c(2,3,4)],1,sum),
                             npp_liana = datum$szpft$npp[,12,c(17)],
                             soil.temp = apply(datum$emean$soil.temp,1,weighted.mean,weights),
                             soil.water = apply(datum$emean$soil.water,1,weighted.mean,weights),
                             leaf.par = datum$emean$leaf.par,
                             leaf.par_tree = apply(datum$szpft$leaf.par[,12,c(2,3,4)],1,sum),
                             leaf.par_liana = datum$szpft$leaf.par[,12,c(17)],
                             fast.soil.c = datum$emean$fast.soil.c,
                             nirup = datum$emean$rshortup - datum$emean$parup/4.6,
                             parup = datum$emean$parup,
                             month = datum$month,
                             year = datum$year,
                             scenario = scenarios[iscenar],
                             simu = isimu)))
      
      config.file <- file.path(directory,paste0(scenarios[iscenar],isimu),"config.xml")
      orient_factor <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "orient_factor")
      clumping <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "clumping_factor")
      leaf_trans_vis <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "leaf_trans_vis")
      leaf_trans_nir <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "leaf_trans_nir")
      leaf_reflect_vis <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "leaf_reflect_vis")
      leaf_reflect_nir <- modify(c(3,17),get_ED_default_pft,xml = config.file,var = "leaf_reflect_nir")
      
      df_params <- bind_rows(list(df_params,
                                  data.frame(orient_factor = orient_factor,
                                             leaf_trans_vis = leaf_trans_vis,
                                             leaf_trans_nir = leaf_trans_nir,
                                             leaf_reflect_vis = leaf_reflect_vis,
                                             leaf_reflect_nir = leaf_reflect_nir,
                                             clumping_factor = clumping,
                                             pft = c("Tree","Liana"),
                                             scenario = scenarios[iscenar],
                                             simu = isimu)))
    } else {
      filter.out <- c(filter.out,isimu)
      print(paste(file.datum," does not exit"))
    }
  }
}

df <- df %>% filter(!(simu %in% filter.out))

# Parameter distributiions
ggplot(data = df_params %>% pivot_longer(-c(pft,scenario,simu))) +
  geom_boxplot(aes(x = scenario, y = value,fill = pft)) +
  facet_wrap(~ name,scales = "free") +
  scale_fill_manual(values = c("#1E64C8","#137300")) +
  theme_bw()


df_long <- df %>% pivot_longer((-c(month,year,simu,scenario)))

df_long <- df_long %>% mutate(pft = case_when(sub(".*\\_", "", name) == "tree" ~ "Tree",
                                                            sub(".*\\_", "", name) == "liana" ~ "Liana",
                                                            TRUE ~ "Ecosystem"),
                              variable = sub("\\_.*", "", name))

tmp <- df_long %>%
  mutate(name = as.factor(name)) %>% group_by(name) %>% summarise(M = summary(aov(formula = value ~ scenario))[[1]][1,5])

# Boxplot comparisons
ggplot(data = df_long) +
  geom_boxplot(aes(y = value, fill = as.factor(scenario))) +
  facet_wrap(~name,scales = "free") +
  theme_bw()

df_sum <- df_long %>% group_by(scenario,month,name) %>% summarise(value_m = mean(value),
                                                                  value_low = quantile(value,0.025),
                                                                  value_high = quantile(value,0.975))

# Seasonal cycle
ggplot(data = df_sum,aes(x = month,fill = scenario,color = scenario)) +
  geom_ribbon(aes(ymin = value_low,ymax = value_high),alpha = 0.2,color = NA) +
  geom_line(aes(y = value_m)) +
  facet_wrap(~name,scales = "free") +
  scale_fill_manual(values = c("#1E64C8","darkgrey")) +
  scale_color_manual(values = c("#1E64C8","darkgrey"))+
  theme_bw()

# Change for NEP
# df_long <- rbind(df_long %>% ungroup() %>% filter(name == "nep") %>% group_by(scenario,year,simu,name) %>% summarise(value = sum(value)/12,
#                                                                                                        month = 1,
#                                                                                                        pft = pft[1],
#                                                                                                        variable = variable[1]) %>% ungroup(),
#                  df_long %>% ungroup() %>% filter(name != "nep"))


df_diff <- df_long %>% group_by(year,month,simu,name) %>%
  summarise(value_diff = value[scenario == scenarios[2]] - value[scenario == scenarios[1]],
                                                                    value_diff_rel = value_diff/abs(value[scenario == scenarios[1]]),
                                                                    pft = pft[scenario == scenarios[1]],
                                                                    variable = variable[scenario == scenarios[1]]) 

df_diff2 <- df_long %>% group_by(scenario,simu,name) %>% summarise(value = mean(value),
                                                                   pft = pft[1],
                                                                   variable = variable[1]) %>% ungroup() %>%
  group_by(simu,name) %>%
  summarise(value_diff = value[scenario == scenarios[2]] - value[scenario == scenarios[1]],
            value_diff_rel = value_diff/abs(value[scenario == scenarios[1]]),
            pft = pft[scenario == scenarios[1]],
            variable = variable[scenario == scenarios[1]]) 

# Predictive interval
PI <- df_diff  %>% group_by(name) %>%filter(abs(value_diff_rel) < 10) %>% summarise(diff_low = quantile(value_diff,0.025,na.rm=TRUE),
                                                                              diff_m = mean(value_diff,na.rm=TRUE),
                                                                              diff_high = quantile(value_diff,0.975,na.rm=TRUE),
                                                                              diff_rel_low = 100*quantile(value_diff_rel,0.025,na.rm=TRUE),
                                                                              diff_rel_m = 100*mean(value_diff_rel,na.rm=TRUE),
                                                                              diff_rel_high = 100*quantile(value_diff_rel,0.975,na.rm=TRUE))

# Confidence interval
CI <- df_diff2 %>% group_by(name,simu) %>% summarise(value_diff = mean(value_diff),
                                                    value_diff_rel = mean(value_diff_rel)) %>% ungroup() %>%
  group_by(name) %>% summarise(diff_low = confint(lm(formula = value_diff ~ 1))[1,1],
                               diff_m = mean(value_diff,na.rm=TRUE),
                               diff_high = confint(lm(formula = value_diff ~ 1))[1,2],
                               diff_rel_low = 100*confint(lm(formula = value_diff_rel ~ 1))[1,1],
                               diff_rel_m = 100*mean(value_diff_rel,na.rm=TRUE),
                               diff_rel_high = 100*confint(lm(formula = value_diff_rel ~ 1))[1,2])

# summary(aov(formula = value ~ as.factor(LI)))[[1]][1,5])

bp.vals <- function(x, probs=c(0.025, 0.25, 0.5,0.75, .975)) {
  r <- quantile(x, probs=probs , na.rm=TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Absolute changes of the selected variables
ggplot(data = df_diff,aes(y = value_diff, x = as.factor(pft), fill = as.factor(pft))) +
  stat_summary(fun.data=bp.vals, geom="boxplot",outlier.shape = NA) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "",fill = "",y = "Absolute change due to lianas") +
  scale_fill_manual(values = c("darkgrey","#1E64C8","#137300")) +
  theme_bw() + guides(fill = FALSE)

# Relative changes of the selected variables
ggplot(data = df_diff,aes(y = value_diff_rel*100, x = as.factor(pft), fill = as.factor(pft))) +
  stat_summary(fun.data=bp.vals, geom="boxplot",outlier.shape = NA) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~variable) +
  labs(x = "",fill = "",y = "Relative change due to lianas") +
  scale_fill_manual(values = c("darkgrey","#1E64C8","#137300")) +
  scale_y_continuous(limits = c(-1,1)*100)+
  theme_bw() + guides(fill = FALSE)

# Carbon cycle
df_CC <- df_diff
df_CC$variable <- factor(df_CC$variable,levels = c("gpp","Rauto","npp","Rhetero","Reco","nep"))

ggplot(data = df_CC %>% filter(!(is.na(variable))),aes(y = value_diff, x = as.factor(pft), fill = as.factor(pft))) +
  stat_summary(fun.data=bp.vals, geom="boxplot",outlier.shape = NA) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~variable) +
  labs(x = "",fill = "",y = "Absolute change due to lianas") +
  scale_fill_manual(values = c("darkgrey","#1E64C8","#137300")) +
  # scale_y_continuous(limits = c(-1,1)*100)+
  theme_bw() + guides(fill = FALSE)

df_pft <- df_long %>% filter(pft != "Ecosystem") %>% group_by(scenario,year,month,simu,variable) %>% summarise(value = value[pft == "Liana"]/(value[pft == "Liana"] + value[pft == "Tree"]))

# Fraction of liana
ggplot(data = df_pft,aes(y = value, x = as.factor(scenario),fill = as.factor(scenario))) +
  stat_summary(fun.data=bp.vals, geom="boxplot",outlier.shape = NA) +
  labs(x = "",fill = "",y = "Fraction of lianas") +
  facet_wrap(~variable) +
  theme_bw() +theme(legend.position = c(0.8,0.1))

df_pft %>% group_by(variable,scenario) %>% summarise(m = mean(value))
df_pft %>% group_by(variable) %>% summarise(pval = summary(aov(value ~ scenario))[[1]][1,5])

###########################################################################################
df_params_wide <- df_params %>% filter(pft == "Liana")

df.sum <- df_long %>% group_by(name,simu,scenario) %>% summarise(value_m = mean(value)) %>% pivot_wider(names_from = name,
                                                                                               values_from = value_m)

params.vs.results <- df_params_wide %>% left_join(df.sum,by = c("simu","scenario"))

plot(params.vs.results$clumping_factor,params.vs.results$gpp)

ggplot(data = params.vs.results) +
  geom_boxplot(aes(x = scenario,y = nirup)) +
  theme_bw()


var2test <- "gpp"
param_names <- colnames(df_params_wide)[1:6]
params.vs.results2test <- params.vs.results %>% dplyr::select(c(param_names,var2test))

anovobj<-aov(lm(as.formula(paste0(var2test," ~ .^2")),data=params.vs.results2test))
allssq<-summary(anovobj)[[1]][,2]
varn <- names((anovobj)$coefficients)[-1]
vars <- param_names

##########
sensivities<-sapply(vars,function(nn){
  
  SSQ <- sum(allssq,na.rm = T)
  SSQ_d <- sum(allssq[which(nn == varn)],na.rm = T)
  SSQ_i <- sum(allssq[which(!(nn == varn) & grepl(nn, varn, fixed=T))],na.rm = T)
  return((SSQ_d+(SSQ_i/2))/SSQ)},USE.NAMES = T) 
sort(sensivities)
