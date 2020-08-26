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
      
      df <- bind_rows(list(df,
                           data.frame(parup = datum$emean$parup/4.6,
                                      rshortup = datum$emean$rshortup,
                                      nirup = datum$emean$rshortup - datum$emean$parup/4.6,
                                      rlongup = datum$emean[["rlongup"]],
                                      par.tot = datum$emean[["par.tot"]]/4.6,
                                      nir.tot = datum$emean[["rshort"]] - datum$emean[["par.tot"]]/4.6,
                                      par.abs = (datum$emean[["par.tot"]] - datum$emean[["parup"]] - datum$emean[["par.gnd"]])/4.6,
                                      par.trans = datum$emean[["par.gnd"]]/4.6,
                                      nir.abs = datum$emean[["rshort"]] - datum$emean[["par.tot"]]/4.6  - (datum$emean[["rshortup"]] - datum$emean[["parup"]]/4.6) - (datum$emean[["rshort.gnd"]] - datum$emean[["par.gnd"]]/4.6),
                                      nir.trans = datum$emean[["rshort.gnd"]] - datum$emean[["par.gnd"]]/4.6,
                                      tir.all = -(datum$emean[["rlong"]] - datum$emean[["rlongup"]]),
                                      rlong.gnd = datum$emean[["rlong.gnd"]],
                                      soil.heat = datum$emean[["rshort.gnd"]] + datum$emean[["rlong.gnd"]],
                                      soil.heat_short = datum$emean[["rshort.gnd"]],
                                      transp = datum$emean$transp,
                                      leaf.temp = datum$emean$leaf.temp,
                                      can.temp = datum$emean$can.temp,
                                      wood.temp = datum$emean$wood.temp,
                                      soil.temp = apply(datum$emean$soil.temp,1,weighted.mean,weights),
                                      latent = datum$emean$qwflxca,
                                      sensible = datum$emean$hflxca,
                                      month = datum$month,
                                      year = datum$year,
                                      scenario = scenarios[iscenar],
                                      simu = isimu)))
    } else {
      filter.out <- c(filter.out,isimu)
      print(paste(file.datum," does not exit"))
    }
  }
}

df %>% dplyr::select(month,year,scenario,simu, par.tot,nir.tot) %>% pivot_longer(cols = -c(month,year,scenario,simu),names_to = "Variable",values_to = "Value") %>% group_by(Variable,scenario) %>%
  summarise(V = mean(Value))

df_diff_temp <- df %>% filter(!(simu %in% filter.out)) %>% pivot_longer(cols = -c(month,year,scenario,simu),names_to = "Variable",values_to = "Value") %>% 
  group_by(scenario,simu,Variable) %>% summarise(Value = mean(Value)) %>% ungroup() %>%
  group_by(Variable,simu) %>% summarise(diff = Value[scenario == scenarios[2]] - Value[scenario == scenarios[1]],
                                        diff_rel = 100*diff/Value[scenario == scenarios[1]])


# Predictive interval
df_diff <- df_diff_temp  %>% group_by(Variable) %>% summarise(diff_low = quantile(diff,0.05,na.rm=TRUE),
                                   diff_m = mean(diff,na.rm=TRUE),
                                   diff_high = quantile(diff,0.95,na.rm=TRUE),
                                   diff_rel_low = quantile(diff_rel,0.05,na.rm=TRUE),
                                   diff_rel_m = mean(diff_rel,na.rm=TRUE),
                                   diff_rel_high = quantile(diff_rel,0.95,na.rm=TRUE))

View(df_diff)

# Confidence interval
CI <- df_diff_temp %>% group_by(Variable) %>% summarise(
  diff_low = confint(lm(formula = diff ~ 1))[1,1],
  diff_m = mean(diff,na.rm=TRUE),
  diff_high = confint(lm(formula = diff ~ 1))[1,2],
  diff_rel_low = confint(lm(formula = diff_rel ~ 1))[1,1],
  diff_rel_m = mean(diff_rel,na.rm=TRUE),
  diff_rel_high = confint(lm(formula = diff_rel ~ 1))[1,2])


df_sum <- df %>% filter(!(simu %in% filter.out)) %>% pivot_longer(cols = -c(month,year,scenario,simu),names_to = "Variable",values_to = "value") %>% group_by(scenario,month,Variable) %>% summarise(value_m = mean(value),
                                                                  value_low = quantile(value,0.025),
                                                                  value_high = quantile(value,0.975))

# Seasonal cycle
ggplot(data = df_sum,aes(x = month,fill = scenario,color = scenario)) +
  geom_ribbon(aes(ymin = value_low,ymax = value_high),alpha = 0.2,color = NA) +
  geom_line(aes(y = value_m)) +
  facet_wrap(~Variable,scales = "free") +
  scale_fill_manual(values = c("#1E64C8","darkgrey")) +
  scale_color_manual(values = c("#1E64C8","darkgrey"))+
  theme_bw()
