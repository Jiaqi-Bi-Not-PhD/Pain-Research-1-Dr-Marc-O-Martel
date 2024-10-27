## Transformation of the outcome variable
data_MED <- read_sav("Bruneau; MED transformed.sav")
summary(data_MED$ComP_Lev2_Aggreg_MED_Tot)
data_MED$ComP_Lev2_Aggreg_MED_Tot_LogTransformed <- log(data_MED$ComP_Lev2_Aggreg_MED_Tot)

data_MED |>
  ggplot(aes(x = ComP_Lev2_Aggreg_MED_Tot)) +
  geom_density()

data_MED |>
  ggplot(aes(x = ComP_Lev2_Aggreg_MED_Tot_LogTransformed)) +
  geom_density()

model_MED_PenIntens <- lmer(ComP_Lev2_Aggreg_MED_Tot_LogTransformed ~ ComP_Lev2_Center_PainIntens + (1|ID), data = data_MED)
summary(model_MED_PenIntens)

model_MED_PCS <- lmer(ComP_Lev2_Aggreg_MED_Tot_LogTransformed ~ ComP_Lev2_Center_PCS + (1|ID), data = data_MED)
summary(model_MED_PCS)

model_MED_OCS <- lmer(ComP_Lev2_Aggreg_MED_Tot_LogTransformed ~ ComP_Lev2_Center_OCS + (1|ID), data = data_MED)
summary(model_MED_OCS)

model_MED_Multivar <- lmer(ComP_Lev2_Aggreg_MED_Tot_LogTransformed ~ ComP_Lev2_Center_OCS + ComP_Lev2_Center_PCS + ComP_Lev2_Center_PainIntens + (1|ID), data = data_MED)
summary(model_MED_Multivar)

model_MED_Multivar <- lmer(ComP_Lev2_Aggreg_MED_Tot ~ ComP_Lev2_Center_OCS + ComP_Lev2_Center_PCS + ComP_Lev2_Center_PainIntens + (1|ID), data = data_MED)
summary(model_MED_Multivar)
