library(tidyverse); library(here)

#THIS IS A TEST

data <- read.table(here::here("data", "data_crosses_2019_Family_Mes_Ano.txt"),sep = "\t", header = T)

data$Ano <- factor(data$Ano)
data$Mes <- factor(data$Mes)
data$Mae <- factor(data$Mae)
data$Pai <- factor(data$Pai)
RmParent <- c("7909-09", "6383", "7909", "7795")
head(data)
library(lme4)
data <- data[!data$Pai%in%RmParent & !data$Mae%in%RmParent,]
library(sjmisc)
library(sjlabelled)

Traits <- colnames(data)[c(11,13,14)]

ResultsFlower <- list()
for(trait in Traits){

model <- lmer(data = data, formula = get(trait) ~ Mes + (1|Mes:Ano) + (1|Mae/Pai))
model.rd <- lmer(data = data, formula = get(trait) ~ Mes + (1|Mes:Ano) + (1|Mae))
model.rd2 <- lmer(data = data, formula = get(trait) ~ Mes + (1|Mes:Ano))
model.rd3 <- lmer(data = data, formula = get(trait) ~ Mes + (1|Mae/Pai))

ResultsFlower[[trait]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlower[[trait]][["LRT"]][[2]] <- anova(model, model.rd3)
ResultsFlower[[trait]][["anova"]] <- anova(model)
ResultsFlower[[trait]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlower[[trait]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
ResultsFlower[[trait]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)
}


BlupsPai <- ResultsFlower[["AbortionRatePond"]][["BLUPS"]][["Pai"]] %>% as.data.frame
BlupsMae <- ResultsFlower[["AbortionRatePond"]][["BLUPS"]][["Mae"]] %>% as.data.frame

for(trait in Traits[-1]){
     BlupsPai <- cbind(BlupsPai, ResultsFlower[[trait]][["BLUPS"]][["Pai"]])
     BlupsMae <- cbind(BlupsMae, ResultsFlower[[trait]][["BLUPS"]][["Mae"]])
}

colnames(BlupsPai) <- c("AborRP", "PercOFer", "NSP")
colnames(BlupsMae) <- c("AborRP", "PercOFer", "NSP")

write.table(BLUPS_Mae, file = here::here("output", "BLUPS_Mae.txt"),sep = "\t", quote = F)
write.table(BLUPS_Pai, file = here::here("output", "BLUPS_Pai.txt"),sep = "\t", quote = F)

sink(here::here("output", "Resultados Analise Anova e Deviance.txt"))
print(ResultsFlower)
sink()

#### Analises de regressao Temperatura, pluviosidade, e umidade 
data <- read.table(here::here("data", "data_crosses_byFamily_decendio_climaticos.txt"),sep = "\t", header = T)



data$ANO <- as.factor(data$ANO)
data$Mes <- as.factor(data$MES)
data$MAE <- as.factor(data$MAE)
data$PAI <- as.factor(data$PAI)
data$Decendio <- as.factor(data$Decendio)
data$DecMes <- as.factor(paste(data$D, data$MES, sep = ":"))
head(data)
library(lme4); library(splines); library(effects); library(ggeffects); library(MuMIn); #library(sjmisc); library(sjlabelled); 

ResultsFlowerWeather <- list()

RainReg <- poly(data$Prec., 2, raw = TRUE)
TMaxReg <- poly(data$T.max, 2, raw = TRUE)
TMeaReg <- poly(data$Tmed, 2, raw = TRUE)
TMinReg <- poly(data$t.min, 2, raw = TRUE)
URReg <- poly(data$U.R., 2, raw = TRUE)
colnames(RainReg) <- c("L.Plu", "Q.Plu")
colnames(TMaxReg) <- c("L.TMax", "Q.TMax")
colnames(TMeaReg) <- c("L.TMea", "Q.TMea")
colnames(TMinReg) <- c("L.TMin", "Q.TMin")
colnames(URReg) <- c("L.UR", "Q.UR")

data2 <- cbind(data, RainReg, TMaxReg, TMeaReg, TMinReg, URReg)
### Decendio effect

#model <- lmer(data = data2, formula = AbortionRate ~ DecMes + (1|DecMes:ANO) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ DecMes + (1|DecMes:ANO) + (1|MAE))
#model.rd2 <- lmer(data = data2, formula = AbortionRate ~ DecMes + (1|DecMes:ANO))
#model.rd3 <- lmer(data = data2, formula = AbortionRate ~ DecMes + (1|MAE/PAI))

#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["LRT"]][[2]] <- anova(model, model.rd3)
#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["Dec"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_Dec_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_Dec_AbR <- as.matrix(ranef(model)$MAE)

model <- lmer(data = data2, formula = AbortionRatePond ~ DecMes + (1|DecMes:ANO) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ DecMes + (1|DecMes:ANO) + (1|MAE))
model.rd2 <- lmer(data = data2, formula = AbortionRatePond ~ DecMes + (1|DecMes:ANO))
model.rd3 <- lmer(data = data2, formula = AbortionRatePond ~ DecMes + (1|MAE/PAI))

ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["LRT"]][[2]] <- anova(model, model.rd3)
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Dec"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["Dec"]]$AbortionRatePond$"Var comp"[4,5]/mean(data$AbortionRatePond)



BlupsPai_Dec_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_Dec_AbRPond <- as.matrix(ranef(model)$MAE)


#model <- lmer(data = data2, formula = PercOvuFertility ~ DecMes + (1|DecMes:ANO) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = PercOvuFertility ~ DecMes + (1|DecMes:ANO) + (1|MAE))
#model.rd2 <- lmer(data = data2, formula = PercOvuFertility ~ DecMes + (1|DecMes:ANO))
#model.rd3 <- lmer(data = data2, formula = PercOvuFertility ~ DecMes + (1|MAE/PAI))

#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["LRT"]][[2]] <- anova(model, model.rd3)
#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["Dec"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_Dec_POF <- as.matrix(ranef(model)$PAI)
#BlupsMae_Dec_POF <- as.matrix(ranef(model)$MAE)


model <- lmer(data = data2, formula = NSeedsPond ~ DecMes + (1|DecMes:ANO) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ DecMes + (1|DecMes:ANO) + (1|MAE))
model.rd2 <- lmer(data = data2, formula = NSeedsPond ~ DecMes + (1|DecMes:ANO))
model.rd3 <- lmer(data = data2, formula = NSeedsPond ~ DecMes + (1|MAE/PAI))

ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["LRT"]][[2]] <- anova(model, model.rd3)
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Dec"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["Dec"]]$NSeedsPond$"Var comp"[4,5]/mean(data$NSeedsPond)

BlupsPai_Dec_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_Dec_NSP <- as.matrix(ranef(model)$MAE)

#### Rain Effect
#model <- lmer(data = data2, formula = AbortionRate ~ poly(L.Plu, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ poly(L.Plu, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = AbortionRate ~ poly(L.Plu, 3)


#model2 <- lmer(data = data2, formula = AbortionRate ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["Rain"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_Rain_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_Rain_AbR <- as.matrix(ranef(model)$MAE)

model <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.Plu, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.Plu, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = AbortionRatePond ~ poly(L.Plu, 3))


model2 <- lmer(data = data2, formula = AbortionRatePond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Rain"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["Rain"]]$AbortionRatePond$"Var comp"[3,5]/mean(data$AbortionRatePond)


BlupsPai_Rain_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_Rain_AbRPond <- as.matrix(ranef(model)$MAE)

eff.Rain.ABR <- ggeffect(model)#, terms = "L.Plu[all]")
eff.Rain.ABR
get_complete_df(eff.Rain.ABR)
mydat.Rain.ABR <- eff.Rain.ABR

#model <- lmer(data = data, formula = PercOvuFertility ~ poly(L.Plu, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data, formula = PercOvuFertility ~ poly(L.Plu, 3) + (1|MAE))
#model.rd2 <- lm(data = data, formula = PercOvuFertility ~ poly(L.Plu, 3))


#model2 <- lmer(data = data2, formula = PercOvuFertility ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["Rain"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_Rain_POF <- as.matrix(ranef(model)$Pai)
#BlupsMae_Rain_POF <- as.matrix(ranef(model)$Mae)

qf(.95, df1=3, df2=488)
model <- lmer(data = data2, formula = NSeedsPond ~ poly(L.Plu, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ poly(L.Plu, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = NSeedsPond ~ poly(L.Plu, 3))


model2 <- lmer(data = data2, formula = NSeedsPond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Rain"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["Rain"]]$NSeeds$"Var comp"[3,5]/mean(data$NSeedsPond)

BlupsPai_Rain_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_Rain_NSP <- as.matrix(ranef(model)$MAE)

eff.Rain.NSP <- ggeffect(model)#, terms = "L.Plu[all]")
eff.Rain.NSP
get_complete_df(eff.Rain.NSP)
mydat.Rain.NSP <- eff.Rain.NSP

### Max Temperature 
#model <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMax, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMax, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = AbortionRate ~ poly(L.TMax, 3))


#model2 <- lmer(data = data2, formula = AbortionRate ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["Tmax"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_TMax_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_TMax_AbR <- as.matrix(ranef(model)$MAE)
# Modelo de Regress?o quadr?tica explica melhor que c?bica######
model <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMax, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMax, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = AbortionRatePond ~ poly(L.TMax, 3))


model2 <- lmer(data = data2, formula = AbortionRatePond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmax"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["Tmax"]]$AbortionRatePond$"Var comp"[3,5]/mean(data$AbortionRatePond)


BlupsPai_TMax_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_TMax_AbRPond <- as.matrix(ranef(model)$MAE)

eff.TMax.ABR <- ggeffect(model)#, terms = "L.TMax[all]")
eff.TMax.ABR
get_complete_df(eff.TMax.ABR)
mydat.TMax.ABR <- eff.TMax.ABR

#model <- lmer(data = data2, formula = PercOvuFertility ~ poly(L.TMax, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = PercOvuFertility ~ poly(L.TMax, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = PercOvuFertility ~ poly(L.TMax, 3))


#model2 <- lmer(data = data2, formula = PercOvuFertility ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["Tmax"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_TMax_POF <- as.matrix(ranef(model)$Pai)
#BlupsMae_TMax_POF <- as.matrix(ranef(model)$Mae)

# Modelo de Regress?o quadr?tica explica melhor que c?bica######

model <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMax, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMax, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = NSeedsPond ~ poly(L.TMax, 3))


model2 <- lmer(data = data2, formula = NSeedsPond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmax"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["Tmax"]]$NSeedsPond$"Var comp"[3,5]/mean(data$NSeedsPond)

BlupsPai_TMax_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_TMax_NSP <- as.matrix(ranef(model)$MAE)

eff.TMax.NSP <- ggeffect(model)#, terms = "L.TMax[all]")
eff.TMax.NSP
get_complete_df(eff.TMax.NSP)
mydat.TMax.NSP <- eff.TMax.NSP

### Min Temperature 
#model <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMin, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMin, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = AbortionRate ~ poly(L.TMin, 3))


#model2 <- lmer(data = data2, formula = AbortionRate ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["Tmin"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_TMin_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_TMin_AbR <- as.matrix(ranef(model)$MAE)

model <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMin, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMin, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = AbortionRatePond ~ poly(L.TMin, 3))


model2 <- lmer(data = data2, formula = AbortionRatePond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmin"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["Tmin"]]$AbortionRatePond$"Var comp"[3,5]/mean(data$AbortionRatePond)




BlupsPai_TMin_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_TMin_AbRPond <- as.matrix(ranef(model)$MAE)

eff.TMin.ABR <- ggeffect(model)#, terms = "L.TMin[all]")
eff.TMin.ABR
get_complete_df(eff.TMin.ABR)
mydat.TMin.ABR <- eff.TMin.ABR

#model <- lmer(data = data, formula = PercOvuFertility ~ poly(L.TMin, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data, formula = PercOvuFertility ~ poly(L.TMin, 3) + (1|MAE))
#model.rd2 <- lm(data = data, formula = PercOvuFertility ~ poly(L.TMin, 3))


#model2 <- lmer(data = data2, formula = PercOvuFertility ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["Tmin"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_TMin_POF <- as.matrix(ranef(model)$Pai)
#BlupsMae_TMin_POF <- as.matrix(ranef(model)$Mae)

# Modelo de Regress?o Quadr?tico explica melhor que o c?bico #############
model <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMin, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMin, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = NSeedsPond ~ poly(L.TMin, 3))


model2 <- lmer(data = data2, formula = NSeedsPond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmin"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["Tmin"]]$NSeedsPond$"Var comp"[3,5]/mean(data$NSeedsPond)


BlupsPai_TMin_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_TMin_NSP <- as.matrix(ranef(model)$MAE)

eff.TMin.NSP <- ggeffect(model)#, terms = "L.TMin[all]")
eff.TMin.NSP
get_complete_df(eff.TMin.NSP)
mydat.TMin.NSP <- eff.TMin.NSP

### Mean Temperature 
#model <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMea, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ poly(L.TMea, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = AbortionRate ~ poly(L.TMea, 3))


#model2 <- lmer(data = data2, formula = AbortionRate ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["Tmed"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_TMea_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_TMea_AbR <- as.matrix(ranef(model)$MAE)

# Modelo de Regress?o Quadr?tic ? superior ao c?bico ##########
model <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMea, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ poly(L.TMea, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = AbortionRatePond ~ poly(L.TMea, 3))


model2 <- lmer(data = data2, formula = AbortionRatePond ~ (1|MAE/PAI))

ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmed"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["Tmed"]]$AbortionRatePond$"Var comp"[3,5]/mean(data$AbortionRatePond)


BlupsPai_TMea_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_TMea_AbRPond <- as.matrix(ranef(model)$MAE)

eff.TMed.ABR <- ggeffect(model)#, terms = "L.TMea[all]")
eff.TMed.ABR
get_complete_df(eff.TMax.ABR)
mydat.TMed.ABR <- eff.TMed.ABR

#model <- lmer(data = data2, formula = PercOvuFertility ~ poly(L.TMea, 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = PercOvuFertility ~ poly(L.TMea, 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = PercOvuFertility ~ poly(L.TMea, 3))


#model2 <- lmer(data = data2, formula = PercOvuFertility ~ (1|MAE/PAI))

#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["Tmed"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_TMea_POF <- as.matrix(ranef(model)$PAI)
#BlupsMae_TMea_POF <- as.matrix(ranef(model)$MAE)

# modelo de regress?o quadr?tico ? superior ao c?bico
model <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMea, 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ poly(L.TMea, 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = NSeedsPond ~ poly(L.TMea, 3))


model2 <- lmer(data = data2, formula = NSeedsPond ~ (1|MAE/PAI))


ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["Tmed"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["Tmed"]]$NSeedsPond$"Var comp"[3,5]/mean(data$NSeedsPond)



BlupsPai_TMea_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_TMea_NSP <- as.matrix(ranef(model)$MAE)

eff.TMed.NSP <- ggeffect(model)#, terms = "L.TMea[all]")
eff.TMed.NSP
get_complete_df(eff.TMed.NSP)
mydat.TMed.NSP <- eff.TMed.NSP

### Relative Humidity
#model <- lmer(data = data2, formula = AbortionRate ~ bs(L.UR, df = 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data2, formula = AbortionRate ~ bs(L.UR, df = 3) + (1|MAE))
#model.rd2 <- lm(data = data2, formula = AbortionRate ~ bs(L.UR, df = 3))


#model2 <- lmer(data = data2, formula = AbortionRate ~ (1|MAE/PAI))

#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["anova"]] <- anova(model, type = 3)
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
#ResultsFlowerWeather[["UR"]][["AbortionRate"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)

#BlupsPai_UR_Abr <- as.matrix(ranef(model)$PAI)
#BlupsMae_UR_AbR <- as.matrix(ranef(model)$MAE)
# verificar alternativa de regress?o quadr?tica ###
model <- lmer(data = data2, formula = AbortionRatePond ~ bs(L.UR, df = 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = AbortionRatePond ~ bs(L.UR, df = 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = AbortionRatePond ~ bs(L.UR, df = 3))


model2 <- lmer(data = data2, formula = AbortionRatePond ~ (1|MAE/PAI))

ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["UR"]][["AbortionRatePond"]][["CV%"]] <- ResultsFlowerWeather[["UR"]]$AbortionRatePond$"Var comp"[3,5]/mean(data$AbortionRatePond)

BlupsPai_UR_AbRPond <- as.matrix(ranef(model)$PAI)
BlupsMae_UR_AbRPond <- as.matrix(ranef(model)$MAE)

eff.UR.ABR <- ggeffect(model)#, terms = "L.UR[all]")
eff.UR.ABR
get_complete_df(eff.UR.ABR)
mydat.UR.ABR <- eff.UR.ABR

#model <- lmer(data = data, formula = PercOvuFertility ~ bs(L.UR, df = 3) + (1|MAE/PAI))
#model.rd <- lmer(data = data, formula = PercOvuFertility ~ bs(L.UR, df = 3) + (1|MAE))
#model.rd2 <- lm(data = data, formula = PercOvuFertility ~ bs(L.UR, df = 3))


#model2 <- lmer(data = data2, formula = PercOvuFertility ~ (1|MAE/PAI))

#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["anova"]] <- anova(model)
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["Var comp"]] <- as.data.frame(VarCorr(model))
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["Beta"]] <- as.matrix(fixef(model))
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["R2"]] <- r.squaredGLMM(model,model2)
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["BLUPS"]][["Mae"]] <- as.matrix(ranef(model)$Mae)
#ResultsFlowerWeather[["UR"]][["PercOvuFertility"]][["BLUPS"]][["Pai"]] <- as.matrix(ranef(model)$Pai)

#BlupsPai_UR_POF <- as.matrix(ranef(model)$Pai)
#BlupsMae_UR_POF <- as.matrix(ranef(model)$Mae)


model <- lmer(data = data2, formula = NSeedsPond ~ bs(L.UR, df = 3) + (1|MAE/PAI))
model.rd <- lmer(data = data2, formula = NSeedsPond ~ bs(L.UR, df = 3) + (1|MAE))
model.rd2 <- lm(data = data2, formula = NSeedsPond ~ bs(L.UR, df = 3))


model2 <- lmer(data = data2, formula = NSeedsPond ~ (1|MAE/PAI))

ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["LRT"]][[1]] <- anova(model,model.rd, model.rd2)
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["anova"]] <- anova(model)
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["Var comp"]] <- as.data.frame(VarCorr(model))
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["Beta"]] <- as.matrix(fixef(model))
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["R2"]] <- r.squaredGLMM(model,model2)
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["BLUPS"]][["MAE"]] <- as.matrix(ranef(model)$MAE)
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["BLUPS"]][["PAI"]] <- as.matrix(ranef(model)$PAI)
ResultsFlowerWeather[["UR"]][["NSeedsPond"]][["CV%"]] <- ResultsFlowerWeather[["UR"]]$NSeedsPond$"Var comp"[3,5]/mean(data$NSeedsPond)

BlupsPai_UR_NSP <- as.matrix(ranef(model)$PAI)
BlupsMae_UR_NSP <- as.matrix(ranef(model)$MAE)

eff.UR.NSP <- ggeffect(model)#, terms = "L.UR[all]")
eff.UR.NSP
get_complete_df(eff.UR.NSP)
mydat.UR.NSP <- eff.UR.NSP

BLUPS_Mae_Weather <- as.data.frame(cbind(BlupsMae_Dec_AbRPond, BlupsMae_Dec_NSP,
				BlupsMae_Rain_AbRPond, BlupsMae_Rain_NSP,
				BlupsMae_TMax_AbRPond, BlupsMae_TMax_NSP,
				BlupsMae_TMin_AbRPond, BlupsMae_TMin_NSP,
				BlupsMae_TMea_AbRPond, BlupsMae_TMea_NSP))

BLUPS_Mae_Weather2 <- as.data.frame(cbind(BlupsMae_UR_AbRPond, BlupsMae_UR_NSP))

colnames(BLUPS_Mae_Weather) <- c("Dec_AbRPond", "Dec_NSP",
				"Rain_AbRPond", "Rain_NSP",
				"TMax_AbRPond", "TMax_NSP",
				"TMin_AbRPond", "TMin_NSP",
				"TMea_AbRPond", "TMea_NSP")

BLUPS_Mae_Weather$Clones <- rownames(BLUPS_Mae_Weather)

colnames(BLUPS_Mae_Weather2) <- c("UR_AbRPond", "UR_NSP")

BLUPS_Mae_Weather2$Clones <- rownames(BLUPS_Mae_Weather2)



BLUPS_Mae_Weather1 <- merge(BLUPS_Mae_Weather, BLUPS_Mae_Weather2, by = "Clones", all = T)



BLUPS_Pai_Weather <- as.data.frame(cbind(BlupsPai_Dec_AbRPond, BlupsPai_Dec_NSP,
				BlupsPai_Rain_AbRPond, BlupsPai_Rain_NSP,
				BlupsPai_TMax_AbRPond, BlupsPai_TMax_NSP,
				BlupsPai_TMin_AbRPond, BlupsPai_TMin_NSP,
				BlupsPai_TMea_AbRPond, BlupsPai_TMea_NSP))

BLUPS_Pai_Weather2 <- as.data.frame(cbind(BlupsPai_UR_AbRPond, BlupsPai_UR_NSP))

colnames(BLUPS_Pai_Weather) <- c("Dec_AbRPond", "Dec_NSP",
				"Rain_AbRPond", "Rain_NSP",
				"TMax_AbRPond", "TMax_NSP",
				"TMin_AbRPond", "TMin_NSP",
				"TMea_AbRPond", "TMea_NSP")
BLUPS_Pai_Weather$Clones <- rownames(BLUPS_Pai_Weather)
colnames(BLUPS_Pai_Weather2) <- c("UR_AbRPond", "UR_NSP")
BLUPS_Pai_Weather2$Clones <- rownames(BLUPS_Pai_Weather2)
BLUPS_Pai_Weather1 <- merge(BLUPS_Pai_Weather, BLUPS_Pai_Weather2, by = "Clones", all = T)


write.table(BLUPS_Mae_Weather1, "BLUPS_Mae_Clima.txt",sep = "\t", quote = F)
write.table(BLUPS_Pai_Weather1, "BLUPS_Pai_Clima.txt",sep = "\t", quote = F)

sink("Resultados An?lise Anova e Deviance com Dados Clim?ticos.txt")
print(ResultsFlowerWeather)
sink()



library(ggplot2); library(ggpubr)

PredPrecABR <- ggplot(mydat.Rain.ABR[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Precipitation (mm)") +
		ylab("Abortion Rate (%)") + ylim(c(69,107)) + theme_minimal()

PredPrecNSP <- ggplot(mydat.Rain.NSP[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Precipitation (mm)") +
		ylab("Seeds Set") + ylim(c(-70,380)) + theme_minimal()

PredTMaxABR <- ggplot(mydat.TMax.ABR[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Max Temperature (?C)") +
		ylab("Abortion Rate (%)") + ylim(c(69,107)) + theme_minimal()

PredTMaxNSP <- ggplot(mydat.TMax.NSP[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Max Temperature (?C)") +
		ylab("Seeds Set") + ylim(c(-70,332)) + theme_minimal()

PredTMinABR <- ggplot(mydat.TMin.ABR[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Min Temperature (?C)") +
		ylab("Abortion Rate (%)") + ylim(c(69,107)) + theme_minimal()

PredTMinNSP <- ggplot(mydat.TMin.NSP[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Min Temperature (?C)") +
		ylab("Seeds Set") + ylim(c(-70,332)) + theme_minimal()

PredTMedABR <- ggplot(mydat.TMed.ABR[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Mean Temperature (?C)") +
		ylab("Abortion Rate (%)") + ylim(c(69,107)) + theme_minimal()

PredTMedNSP <- ggplot(mydat.TMed.NSP[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Mean Temperature (?C)") +
		ylab("Seeds Set") + ylim(c(-75,332)) + theme_minimal()

PredURABR <- ggplot(mydat.UR.ABR[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Relative Humidity (%)") +
		ylab("Abortion Rate (%)") + ylim(c(69,107)) + theme_minimal()

PredURNSP <- ggplot(mydat.UR.NSP[[1]], aes(x = x, y = predicted)) +
		geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + geom_smooth(method = "loess") + xlab("Relative Humidity (%)") +
		ylab("Seeds Set") + ylim(c(-70,332)) + theme_minimal()

JointGraph <- ggarrange(PredPrecABR, PredPrecNSP,
		PredTMaxABR, PredTMaxNSP,
		PredTMedABR, PredTMedNSP,
		PredTMinABR, PredTMinNSP,
		PredURABR, PredURNSP, ncol = 2, nrow = 5)
JointGraph

tiff(file = "Regress?es Cubicas para dados clim?ticos.tiff", res = 300, unit = "cm", width = 20, height = 30)
JointGraph
dev.off()

JointGraph2 <- ggarrange(PredPrecNSP, PredTMaxNSP,
				 PredTMedNSP, PredTMinNSP, 
				 PredTMaxABR, ncol = 2, nrow = 3)
JointGraph2
dev.off()
tiff(file = "Regress?es Cubicas para dados clim?ticos Seed Set.tiff", res = 500, unit = "cm", width = 16, height = 21.6, compression = "lzw")
JointGraph2
dev.off()

pdf(file = "Regress?es Cubicas para dados clim?ticos Seed Set.pdf", width = 8, height = 10.8)
JointGraph2
dev.off()


library(ggpubr)
PreAR <- ggplot(data2, aes(x = Prec., y = AbortionRatePond)) +
geom_smooth(method = "loess") + ylim(c(85,100)) + theme_minimal() + xlab("Precipitation (mm)") +
ylab("Abortion Rate (%)")

PreNS <- ggplot(data2, aes(x = Prec., y = NSeedsPond)) +
geom_smooth(method = "loess") + ylim(c(0,200)) + theme_minimal() + xlab("Precipitation (mm)") +
ylab("Seed Set Weighted")

TMaxAR <- ggplot(data2, aes(x = T.max, y = AbortionRatePond)) +
geom_smooth(method = "loess") + ylim(c(85,100)) + theme_minimal() + xlab("Max Temperature (?C)") +
ylab("Abortion Rate (%)")

TMaxNS <- ggplot(data2, aes(x = T.max, y = NSeedsPond)) +
geom_smooth(method = "loess") + ylim(c(0,200)) + theme_minimal() + xlab("Max Temperature (?C)") +
ylab("Seed Set Weighted")

TMinAR <- ggplot(data2, aes(x = t.min, y = AbortionRatePond)) +
geom_smooth(method = "loess") + ylim(c(85,100)) + theme_minimal() + xlab("Min Temperature (?C)") +
ylab("Abortion Rate (%)")

TMinNS <- ggplot(data2, aes(x = t.min, y = NSeedsPond)) +
geom_smooth(method = "loess") + ylim(c(0,200)) + theme_minimal() + xlab("Min Temperature (?C)") +
ylab("Seed Set Weighted")

TMedAR <- ggplot(data2, aes(x = Tmed, y = AbortionRatePond)) +
geom_smooth(method = "loess") + ylim(c(85,100)) + theme_minimal() + xlab("Mean Temperature (?C)") +
ylab("Abortion Rate (%)")

TMedNS <- ggplot(data2, aes(x = Tmed, y = NSeedsPond)) +
geom_smooth(method = "loess") + xlim(c(22,26)) + ylim(c(0,200)) + theme_minimal() + xlab("Mean Temperature (?C)") +
ylab("Seed Set Weighted")

URAR <- ggplot(data2, aes(x = U.R., y = AbortionRatePond)) +
geom_smooth(method = "loess") + ylim(c(85,100)) + theme_minimal() + xlab("Relative Humidity (%)") +
ylab("Abortion Rate (%)")

URNS <- ggplot(data2, aes(x = U.R., y = NSeedsPond)) +
geom_smooth(method = "loess") + ylim(c(0,200)) + theme_minimal() + xlab("Relative Humidity (%)") +
ylab("Seed Set Weighted")


JointGrap <- ggarrange(PreAR, PreNS,
		TMaxAR, TMaxNS,
		TMedAR, TMedNS,
		TMinAR, TMinNS,
		URAR, URNS, ncol = 2, nrow = 5)
JointGrap

tiff(file = "Regress?es loess para dados clim?ticos.tiff", res = 300, unit = "cm", width = 20, height = 30)
JointGrap
dev.off()

JointGrap <- ggarrange(PreNS, TMaxNS,
                       TMedNS, TMinNS,
                       TMaxAR, ncol = 2, nrow = 3)
JointGrap
pdf(file = "Regress?es loess para dados clim?ticos.pdf", width = 20, height = 30)
JointGrap
dev.off()

### An?lise de Componentes Principais ###

DadosPC <- read.table("BLUPS_Mae.txt", header = T, sep = "\t")
PCResults <- prcomp(DadosPC[,-2])
PCPoints <- as.data.frame(PCResults$x)
dim(PCPoints)
ggplot(PCPoints, aes(x = PC1, y = PC2)) + geom_point() + theme_minimal()

### Analise Dialelica Cruzamentos ###


### Sommer Mixed Models ###
library(sommer); library(plyr); library(emmeans)


DadosDiallel <- read.table("Analises Microscopia.txt", header = T, sep = "\t")
Parentais <- data.frame(ordem = order(unique(c(as.character(DadosDiallel$M?e),as.character(DadosDiallel$Pai))), decreasing = TRUE), Clone = unique(c(as.character(DadosDiallel$M?e),as.character(DadosDiallel$Pai))))
for(i in 1:nrow(DadosDiallel)){
DadosDiallel$CodM[i] <- Parentais$ordem[Parentais$Clone%in%as.character(DadosDiallel$M?e[i])]
DadosDiallel$CodP[i] <- Parentais$ordem[Parentais$Clone%in%as.character(DadosDiallel$Pai[i])]
if(DadosDiallel$CodM[i] <= DadosDiallel$CodP[i]){
DadosDiallel$CEC[i] <- paste(DadosDiallel$CodM[i],DadosDiallel$CodP[i], sep = ":")
 } else {DadosDiallel$CEC[i] <- paste(DadosDiallel$CodP[i],DadosDiallel$CodM[i], sep = ":")
}
if(DadosDiallel$CodM[i] <= DadosDiallel$CodP[i]){
	DadosDiallel$Cross[i] <- "Hyb"
	} else {DadosDiallel$Cross[i] <- "Rec"}
}

CEC2 <- do.call(rbind, strsplit(DadosDiallel$CEC, split = ":"))
CEC2 <- data.frame(matrix(as.numeric(CEC2), nrow = 1150, ncol = 2))
colnames(CEC2) <- c("ordem", "ordem2")
CEC2$ordem <- as.factor(CEC2$ordem)
CEC2$ordem2 <- as.factor(CEC2$ordem2)

CEC2$ordem <- mapvalues(CEC2$ordem, from = as.character(Parentais$ordem[Parentais$ordem%in%levels(CEC2$ordem)]), to = as.character(Parentais$Clone[Parentais$ordem%in%levels(CEC2$ordem)]))
CEC2$ordem2 <- mapvalues(CEC2$ordem2, from = as.character(Parentais$ordem), to = as.character(Parentais$Clone))



DadosDiallel$CEC2 <- paste(CEC2$ordem, CEC2$ordem2, sep = ":")
ParSel <- read.table("Parentais Sel.txt", header = T, sep = "\t")
DadosDiallel <- DadosDiallel[(DadosDiallel$M?e%in%ParSel$Parentais & DadosDiallel$Pai%in%ParSel$Parentais),]
#ClusterPar <- read.table("clustersParentais.txt", header = T, sep = "\t")
#ClusterPar <- ClusterPar[,c(1,5)]
DadosDiallel2 <- merge(DadosDiallel, ParSel, by.x = "M?e", by.y = "Parentais")
DadosDiallel2 <- merge(DadosDiallel2, ParSel, by.x = "Pai", by.y = "Parentais")
#colnames(DadosDiallel2)[15:16] <- c("Group1", "Group2")
for(i in 1:nrow(DadosDiallel2)){
if(DadosDiallel2$Group1[i] <= DadosDiallel2$Group2[i]){
DadosDiallel2$CEC3[i] <- paste(DadosDiallel2$Group1[i],DadosDiallel2$Group2[i], sep = ":")
 } else {DadosDiallel2$CEC3[i] <- paste(DadosDiallel2$Group2[i],DadosDiallel2$Group1[i], sep = ":")}
}
DadosDiallel2$Group1 <- as.factor(DadosDiallel2$Group1)
DadosDiallel2$Group2 <- as.factor(DadosDiallel2$Group2)
DadosDiallel2$CEC3 <- as.factor(DadosDiallel2$CEC3)

# Table for Supplementary tables

data <- read.table("data_crosses_byFamily_decendio_climaticos.txt",sep = "\t", header = T)

DadosDiallel2$Comb <- paste(DadosDiallel2$Pai,DadosDiallel2$M?e, sep = ":")
data$Comb <- paste(data$PAI, data$MAE, sep = ":")
table1 <- cbind(table(DadosDiallel2$Comb, DadosDiallel2$GPD),
                table(DadosDiallel2$Comb, DadosDiallel2$GPG),
                table(DadosDiallel2$Comb, DadosDiallel2$LPTP),
                table(DadosDiallel2$Comb, DadosDiallel2$NOF))
table2 <- table(DadosDiallel2$Pai, DadosDiallel2$GPD)
unique(DadosDiallel2$Pai[!DadosDiallel2$Pai%in%data$PAI])
write.table(unique(DadosDiallel2$Comb[DadosDiallel2$Comb%in%data$Comb]), file = "Combina??es entre experimentos.txt", quote = F, sep = "\t")
unique(DadosDiallel2$Comb[!DadosDiallel2$Comb%in%data$Comb])
unique(data$Comb[!data$Comb%in%DadosDiallel2$Comb])
write.table(table1, file = "Tabela resumida da analise de microscopia.txt", sep = "\t", quote = F)
write.table(table2, file = "Tabela resumida da aderencia de grao de polen pai.txt", sep = "\t", quote = F)

# Fit the model


DadosDiallel$Informa??o <- factor(DadosDiallel$Informa??o, levels = c("Pre Anthesis", "Anthesis", "Pos Anthesis"))


AnovaDiallel <- matrix(NA, nrow = 8, ncol = 5)
rownames(AnovaDiallel) <- c(paste(rep(c("GPD", "GPG", "LPTP", "NOF"), each = 2), rep(c("GPD", "Informa??o"), times = 4), sep = ":"))
colnames(AnovaDiallel) <- c("Df", "Sum.Sq", "Mean.Sq", "F.value", "Pr(>F)")

LRTDiallel <- matrix(NA, nrow = 8, ncol = 7)
rownames(LRTDiallel) <- c(paste(rep(c("GPD", "GPG", "LPTP", "NOF"), each = 2), rep(c("GGC", "CEC"), times = 4), sep = ":"))
colnames(LRTDiallel) <- c("Df", "AIC", "BIC", "loLik", "Chisq", "ChiDf", "PrChisq")

VarCompDiallel <- matrix(NA, nrow = 7, ncol = 4)
rownames(VarCompDiallel) <- c("VarCGC", "VarCEC", "VarRes", "VarAdd", "VarGen", "h2", "H2")
colnames(VarCompDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

FixEffDiallel <- list()

CGCEffDiallel <- matrix(NA, nrow = 21, ncol = 4)
colnames(CGCEffDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

CECEffDiallel <- matrix(NA, nrow = 130, ncol = 4)
colnames(CECEffDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

####GPD
modHD <- mmer(GPD ~ Informa??o, 
               random = ~ vs(overlay(M?e,Pai)) + vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel, method = "AI")
modHD2 <- mmer(GPD ~ Informa??o, 
               random = ~ vs(overlay(M?e,Pai)),
		   rcov= ~ vs(units),
               data = DadosDiallel, method = "AI")
modHD3 <- mmer(GPD ~ Informa??o, 
               random = ~ vs(CEC2),
		   rcov=~ vs(units),
               data = DadosDiallel, method = "AI")

AnovaDiallel[2,] <- as.matrix(anova(modHD)[2,1:5])

LRTDiallel[1,] <- as.matrix(anova.mmer(modHD3,modHD)[2,1:7])
LRTDiallel[2,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,1] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,1] <- VarCompDiallel[1,1]*2
VarCompDiallel[5,1] <- VarCompDiallel[1,1]*2 + VarCompDiallel[2,1]
VarCompDiallel[6,1] <- (VarCompDiallel[4,1]/(VarCompDiallel[5,1] + VarCompDiallel[3,1]))
VarCompDiallel[7,1] <- (VarCompDiallel[5,1]/(VarCompDiallel[5,1] + VarCompDiallel[3,1]))

FixEffDiallel[["GPD"]][["Informa??o"]] <- emmeans(modHD, list(pairwise ~ Informa??o), adjust = "tukey")

CGCEffDiallel[,1] <- as.matrix(randef(modHD)[[1]][[1]])
rownames(CGCEffDiallel) <- rownames(as.matrix(randef(modHD)[[1]][[1]]))

CECEffDiallel[,1] <- as.matrix(randef(modHD)[[2]][[1]])
rownames(CECEffDiallel) <- rownames(as.matrix(randef(modHD)[[2]][[1]]))


DadosDiallel$GPD <- factor(DadosDiallel$GPD, levels = c("1", "2", "3"))
traits <- c("GPG", "LPTP", "NOF")

####GPG
modHD <- mmer(GPG ~ Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)) + vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel, method = "AI")
modHD2 <- mmer(GPG ~ Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)),
		   rcov= ~ vs(units),
               data=DadosDiallel, method = "AI")
modHD3 <- mmer(GPG~Informa??o + GPD, 
               random=~ vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel, method = "AI")


AnovaDiallel[3:4,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[3,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[4,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,2] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,2] <- VarCompDiallel[1,2]*2
VarCompDiallel[5,2] <- VarCompDiallel[1,2]*2 + VarCompDiallel[2,2]
VarCompDiallel[6,2] <- (VarCompDiallel[4,2]/(VarCompDiallel[5,2] + VarCompDiallel[3,2]))
VarCompDiallel[7,2] <- (VarCompDiallel[5,2]/(VarCompDiallel[5,2] + VarCompDiallel[3,2]))

FixEffDiallel[["GPG"]][["Informa??o"]] <- emmeans(modHD, list(pairwise ~ Informa??o), adjust = "tukey")
FixEffDiallel[["GPG"]][["GPD"]] <- emmeans(modHD, list(pairwise ~ GPD), adjust = "tukey")

CGCEffDiallel[,2] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,2] <- as.matrix(randef(modHD)[[2]][[1]])

#### LPTP
modHD <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)) + vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel, method = "AI")
modHD2 <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)),
		   rcov= ~ vs(units),
               data=DadosDiallel, method = "AI")
modHD3 <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel, method = "AI")

AnovaDiallel[5:6,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[5,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[6,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,3] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,3] <- VarCompDiallel[1,3]*2
VarCompDiallel[5,3] <- VarCompDiallel[1,3]*2 + VarCompDiallel[2,3]
VarCompDiallel[6,3] <- (VarCompDiallel[4,3]/(VarCompDiallel[5,3] + VarCompDiallel[3,3]))
VarCompDiallel[7,3] <- (VarCompDiallel[5,3]/(VarCompDiallel[5,3] + VarCompDiallel[3,3]))

FixEffDiallel[["LPTP"]][["Informa??o"]] <- emmeans(modHD, list(pairwise ~ Informa??o), adjust = "tukey")
FixEffDiallel[["LPTP"]][["GPD"]] <- emmeans(modHD, list(pairwise ~ GPD), adjust = "tukey")

CGCEffDiallel[,3] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,3] <- as.matrix(randef(modHD)[[2]][[1]])

#### NOF
modHD <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)) + vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel)
modHD2 <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(overlay(M?e,Pai)),
		   rcov= ~ vs(units),
               data=DadosDiallel)
modHD3 <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(CEC2),
		   rcov=~ vs(units),
               data=DadosDiallel)

AnovaDiallel[7:8,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[7,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[8,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,4] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,4] <- VarCompDiallel[1,4]*2
VarCompDiallel[5,4] <- VarCompDiallel[1,4]*2 + VarCompDiallel[2,4]
VarCompDiallel[6,4] <- (VarCompDiallel[4,4]/(VarCompDiallel[5,4] + VarCompDiallel[3,4]))
VarCompDiallel[7,4] <- (VarCompDiallel[5,4]/(VarCompDiallel[5,4] + VarCompDiallel[3,4]))

FixEffDiallel[["NOF"]][["Informa??o"]] <- emmeans(modHD, list(pairwise ~ Informa??o), adjust = "tukey")
FixEffDiallel[["NOF"]][["GPD"]] <- emmeans(modHD, list(pairwise ~ GPD), adjust = "tukey")

CGCEffDiallel[,4] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,4] <- as.matrix(randef(modHD)[[2]][[1]])

ResultsDAPC <- list()

ResultsDAPC$Anova <- AnovaDiallel
ResultsDAPC$LRT <- LRTDiallel
ResultsDAPC$VarComp <- VarCompDiallel
ResultsDAPC$FixEff <- FixEffDiallel
ResultsDAPC$RandEff$CGC <- CGCEffDiallel
ResultsDAPC$RandEff$CEC <- CECEffDiallel

sink('Resultados An?lise Dados Microsc?pio.txt')
print(ResultsDAPC)
sink()

library(sommer); library(plyr)


DadosDiallel <- read.table("Analises Microscopia.txt", header = T, sep = "\t")
Parentais <- data.frame(ordem = order(unique(c(as.character(DadosDiallel$M?e),as.character(DadosDiallel$Pai))), decreasing = TRUE), Clone = unique(c(as.character(DadosDiallel$M?e),as.character(DadosDiallel$Pai))))
for(i in 1:nrow(DadosDiallel)){
DadosDiallel$CodM[i] <- Parentais$ordem[Parentais$Clone%in%as.character(DadosDiallel$M?e[i])]
DadosDiallel$CodP[i] <- Parentais$ordem[Parentais$Clone%in%as.character(DadosDiallel$Pai[i])]
if(DadosDiallel$CodM[i] <= DadosDiallel$CodP[i]){
DadosDiallel$CEC[i] <- paste(DadosDiallel$CodM[i],DadosDiallel$CodP[i], sep = ":")
 } else {DadosDiallel$CEC[i] <- paste(DadosDiallel$CodP[i],DadosDiallel$CodM[i], sep = ":")
}
if(DadosDiallel$CodM[i] <= DadosDiallel$CodP[i]){
	DadosDiallel$Cross[i] <- "Hyb"
	} else {DadosDiallel$Cross[i] <- "Rec"}
}

CEC2 <- do.call(rbind, strsplit(DadosDiallel$CEC, split = ":"))
CEC2 <- data.frame(matrix(as.numeric(CEC2), nrow = 1150, ncol = 2))
colnames(CEC2) <- c("ordem", "ordem2")
CEC2$ordem <- as.factor(CEC2$ordem)
CEC2$ordem2 <- as.factor(CEC2$ordem2)

CEC2$ordem <- mapvalues(CEC2$ordem, from = as.character(Parentais$ordem[Parentais$ordem%in%levels(CEC2$ordem)]), to = as.character(Parentais$Clone[Parentais$ordem%in%levels(CEC2$ordem)]))
CEC2$ordem2 <- mapvalues(CEC2$ordem2, from = as.character(Parentais$ordem), to = as.character(Parentais$Clone))



DadosDiallel$CEC2 <- paste(CEC2$ordem, CEC2$ordem2, sep = ":")
ParSel <- read.table("Parentais Sel.txt", header = T, sep = "\t")
DadosDiallel <- DadosDiallel[(DadosDiallel$M?e%in%ParSel$Parentais & DadosDiallel$Pai%in%ParSel$Parentais),]
ClusterPar <- read.table("GrupoHeterotico.txt", header = T, sep = "\t")
#ClusterPar <- ClusterPar[,c(1,5)]
DadosDiallel2 <- merge(DadosDiallel, ClusterPar, by.x = "M?e", by.y = "Clone")
DadosDiallel2 <- merge(DadosDiallel2, ClusterPar, by.x = "Pai", by.y = "Clone")
colnames(DadosDiallel2)[15:16] <- c("Group1", "Group2")
for(i in 1:nrow(DadosDiallel2)){
if(DadosDiallel2$Group1[i] <= DadosDiallel2$Group2[i]){
DadosDiallel2$CEC3[i] <- paste(DadosDiallel2$Group1[i],DadosDiallel2$Group2[i], sep = ":")
 } else {DadosDiallel2$CEC3[i] <- paste(DadosDiallel2$Group2[i],DadosDiallel2$Group1[i], sep = ":")}
}
DadosDiallel2$Group1 <- as.factor(DadosDiallel2$Group1)
DadosDiallel2$Group2 <- as.factor(DadosDiallel2$Group2)
DadosDiallel2$CEC3 <- as.factor(DadosDiallel2$CEC3)


library(tidyverse)
DadosAutFec <- filter(DadosDiallel2, as.character(Pai) == as.character(M?e))
table(as.character(DadosAutFec$Pai), DadosAutFec$LPTP)
# Fit the model


DadosDiallel2$Informa??o <- factor(DadosDiallel2$Informa??o, levels = c("Pre Anthesis", "Anthesis", "Pos Anthesis"))


AnovaDiallel <- matrix(NA, nrow = 8, ncol = 5)
rownames(AnovaDiallel) <- c(paste(rep(c("GPD", "GPG", "LPTP", "NOF"), each = 2), rep(c("GPD", "Informa??o"), times = 4), sep = ":"))
colnames(AnovaDiallel) <- c("Df", "Sum.Sq", "Mean.Sq", "F.value", "Pr(>F)")

LRTDiallel <- matrix(NA, nrow = 8, ncol = 7)
rownames(LRTDiallel) <- c(paste(rep(c("GPD", "GPG", "LPTP", "NOF"), each = 2), rep(c("GGC", "CEC"), times = 4), sep = ":"))
colnames(LRTDiallel) <- c("Df", "AIC", "BIC", "loLik", "Chisq", "ChiDf", "PrChisq")

VarCompDiallel <- matrix(NA, nrow = 7, ncol = 4)
rownames(VarCompDiallel) <- c("VarCGC", "VarCEC", "VarRes", "VarAdd", "VarGen", "h2", "H2")
colnames(VarCompDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

FixEffDiallel <- matrix(NA, nrow = 5, ncol = 4)
colnames(FixEffDiallel) <- c("GPD", "GPG", "LPTP", "NOF")
rownames(FixEffDiallel) <- c("Pre Anthesis:LowGPD", "Anthesis", "Pos Anthesis", "MedianGPD", "HighGPD")

CGCEffDiallel <- matrix(NA, nrow = 3, ncol = 4)
colnames(CGCEffDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

CECEffDiallel <- matrix(NA, nrow = 6, ncol = 4)
colnames(CECEffDiallel) <- c("GPD", "GPG", "LPTP", "NOF")

####GPD
modHD <- mmer(GPD~Informa??o, 
               random=~ vs(Group1) + vs(Group2) + vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI", tolparinv = 1e-05)
modHD2 <- mmer(GPD~Informa??o, 
               random=~ vs(Group1) + vs(Group2),
		   rcov= ~ vs(units),
               data=DadosDiallel2, method = "AI")
modHD3 <- mmer(GPD~Informa??o, 
               random=~ vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI")

AnovaDiallel[2,] <- as.matrix(anova(modHD)[2,1:5])

LRTDiallel[1,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[2,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,1] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,1] <- VarCompDiallel[1,1]*4
VarCompDiallel[5,1] <- VarCompDiallel[1,1]*4 + VarCompDiallel[2,1]*4
VarCompDiallel[6,1] <- (VarCompDiallel[4,1]/(VarCompDiallel[5,1] + VarCompDiallel[3,1]))
VarCompDiallel[7,1] <- (VarCompDiallel[5,1]/(VarCompDiallel[5,1] + VarCompDiallel[3,1]))

FixEffDiallel[1:3,1] <- as.matrix(modHD$Beta$Estimate)

CGCEffDiallel[,1] <- as.matrix(randef(modHD)[[1]][[1]])
rownames(CGCEffDiallel) <- rownames(as.matrix(randef(modHD)[[1]][[1]]))

CECEffDiallel[,1] <- as.matrix(randef(modHD)[[2]][[1]])
rownames(CECEffDiallel) <- rownames(as.matrix(randef(modHD)[[2]][[1]]))


DadosDiallel2$GPD <- factor(DadosDiallel2$GPD, levels = c("1", "2", "3"))
traits <- c("GPG", "LPTP", "NOF")

####GPG
modHD <- mmer(GPG~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2) + vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI")
modHD2 <- mmer(GPG~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2),
		   rcov= ~ vs(units),
               data=DadosDiallel2, method = "AI")
modHD3 <- mmer(GPG~Informa??o + GPD, 
               random=~ vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI")

AnovaDiallel[3:4,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[3,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[4,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,2] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,2] <- VarCompDiallel[1,2]*4
VarCompDiallel[5,2] <- VarCompDiallel[1,2]*4 + VarCompDiallel[2,2]*4
VarCompDiallel[6,2] <- (VarCompDiallel[4,2]/(VarCompDiallel[5,2] + VarCompDiallel[3,2]))
VarCompDiallel[7,2] <- (VarCompDiallel[5,2]/(VarCompDiallel[5,2] + VarCompDiallel[3,2]))

FixEffDiallel[1:5,2] <- as.matrix(modHD$Beta$Estimate)

CGCEffDiallel[,2] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,2] <- as.matrix(randef(modHD)[[2]][[1]])

#### LPTP
modHD <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2) + vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI")
modHD2 <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2),
		   rcov= ~ vs(units),
               data=DadosDiallel2, method = "AI")
modHD3 <- mmer(LPTP~Informa??o + GPD, 
               random=~ vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2, method = "AI")

AnovaDiallel[5:6,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[5,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[6,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,3] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,3] <- VarCompDiallel[1,3]*4
VarCompDiallel[5,3] <- VarCompDiallel[1,3]*4 + VarCompDiallel[2,3]*4
VarCompDiallel[6,3] <- (VarCompDiallel[4,3]/(VarCompDiallel[5,3] + VarCompDiallel[3,3]))
VarCompDiallel[7,3] <- (VarCompDiallel[5,3]/(VarCompDiallel[5,3] + VarCompDiallel[3,3]))

FixEffDiallel[1:5,3] <- as.matrix(modHD$Beta$Estimate)

CGCEffDiallel[,3] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,3] <- as.matrix(randef(modHD)[[2]][[1]])

#### NOF
modHD <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2) + vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2)
modHD2 <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(Group1) + vs(Group2),
		   rcov= ~ vs(units),
               data=DadosDiallel2)
modHD3 <- mmer(NOF~Informa??o + GPD, 
               random=~ vs(CEC3),
		   rcov=~ vs(units),
               data=DadosDiallel2)

AnovaDiallel[7:8,] <- as.matrix(anova(modHD)[2:3,1:5])

LRTDiallel[7,] <- as.matrix(anova(modHD3,modHD)[2,1:7])
LRTDiallel[8,] <- as.matrix(anova(modHD2,modHD)[2,1:7])

VarCompDiallel[1:3,4] <- as.matrix(c(summary(modHD)$varcomp[,1]))
VarCompDiallel[4,4] <- VarCompDiallel[1,4]*4
VarCompDiallel[5,4] <- VarCompDiallel[1,4]*4 + VarCompDiallel[2,4]*4
VarCompDiallel[6,4] <- (VarCompDiallel[4,4]/(VarCompDiallel[5,4] + VarCompDiallel[3,4]))
VarCompDiallel[7,4] <- (VarCompDiallel[5,4]/(VarCompDiallel[5,4] + VarCompDiallel[3,4]))

FixEffDiallel[1:5,4] <- as.matrix(modHD$Beta$Estimate)

CGCEffDiallel[,4] <- as.matrix(randef(modHD)[[1]][[1]])

CECEffDiallel[,4] <- as.matrix(randef(modHD)[[2]][[1]])

ResultsDAPC <- list()

ResultsGpHet$Anova <- AnovaDiallel
ResultsGpHet$LRT <- LRTDiallel
ResultsGpHet$VarComp <- VarCompDiallel
ResultsGpHet$FixEff <- FixEffDiallel
ResultsGpHet$RandEff$CGC <- CGCEffDiallel
ResultsGpHet$RandEff$CEC <- CECEffDiallel

sink('Resultados An?lise Grupo Heter?tico UPGMA.txt')
print(ResultsGpHet)
sink()

#### Agrupamento


LPTP <- do.call(rbind, strsplit(rownames(CECEffDiallel), split = ":"))
LPTP2 <- data.frame(LPTP, CECEffDiallel)
colnames(LPTP2)[1:2] <- c("Cross1", "Cross2")
Diallel.measures1 <- c("GCG", "LPTP2")


LPTP2$Cross1 <- as.character(LPTP2$Cross1)
LPTP2$Cross2 <- as.character(LPTP2$Cross2)

LPTP3 <- as.matrix(LPTP2[!LPTP2$Cross1==LPTP2$Cross2,4:5])
Dist <- dist(LPTP3)

fulldf <- expand.grid(Cross1=unique(LPTP2$Cross1),Cross2=unique(LPTP2$Cross2),stringsAsFactors = F)

mdf <- merge(fulldf,LPTP2,by.x=c("Cross1","Cross2"),by.y=c("Cross1","Cross2"),all.x=T)

distmat <- as.matrix(xtabs(LPTP~Cross1+Cross2,mdf,na.action=na.pass))

distmat[is.na(distmat)] <- 9

distmat <- as.matrix(xtabs(LPTP~Cross1+Cross2,LPTP2,na.action=na.pass))

distmat[distmat==0] <- 9
distmat2 <- dist(distmat)
plot(as.dendrogram(hclust(Dist)))


### An?lise Dial?lica Multivariada ###
modHD <- mmer(cbind(GPD, GPG, LPTP, NOF)~Informa??o, 
               random=~ vs(overlay(M?e,Pai), Gu = A, Gtc=unsm(4)) + vs(CEC2, Gu = AA, Gtc=unsm(4)),
		   rcov= ~ vs(units, Gtc=unsm(4)),
               data=DadosDiallel2, tolparinv = 1e-03, method = "AI")

modHD2 <- mmer(cbind(GPD, GPG, LPTP, NOF)~Informa??o, 
               random=~ vs(CEC2, Gu = AA, Gtc=unsm(4)),
		   rcov= ~ vs(units, Gtc=unsm(4)),
               data=DadosDiallel2, tolparinv = 1e-03, method = "AI")

modHD3 <- mmer(cbind(GPD, GPG, LPTP, NOF)~Informa??o, 
               random=~ vs(overlay(M?e,Pai), Gu = A, Gtc=unsm(4)),
		   rcov= ~ vs(units, Gtc=unsm(4)),
               data=DadosDiallel2, tolparinv = 1e-03, method = "AI")

5h 17m 42s

CVCGC <- matrix(modHD$sigma[[1]], nrow = 4)
colnames(CVCGC) <- colnames(modHD$sigma[[1]])
rownames(CVCGC) <- colnames(modHD$sigma[[1]])
CVCEC <- matrix(modHD$sigma[[2]], nrow = 4)
colnames(CVCEC) <- colnames(modHD$sigma[[1]])
rownames(CVCEC) <- colnames(modHD$sigma[[1]])
CVRes <- matrix(modHD$sigma[[3]], nrow = 4)
colnames(CVRes) <- colnames(modHD$sigma[[1]])
rownames(CVRes) <- colnames(modHD$sigma[[1]])
