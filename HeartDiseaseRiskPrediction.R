#---
#title: "PBHL340_FinalProject"
#date: "2025-12-10"
#---

getwd()
setwd("/Users/nishkapatel/Downloads")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(broom)
heart <- read_csv("heart_disease_uci.csv", na = c("", "NA", "?"))
View(heart)
names(heart)
glimpse(heart)

#create outcome variable 
heart <- heart %>%
  mutate(
    num = as.numeric(num),
    disease = if_else(num > 0, "Disease", "No disease"),
    disease = factor(disease, levels = c("No disease", "Disease"))
  )

#convert categorical variables
heart <- heart %>%
  mutate(
    sex     = factor(sex),
    cp      = factor(cp),       
    fbs     = factor(fbs),
    exang   = factor(exang),
    restecg = factor(restecg),
    slope   = factor(slope),
    ca      = as.numeric(ca),
    thal    = factor(thal)
  )

#chi-square tests: 

#disease vs. cp
heart_cp <- heart %>% 
  filter(!is.na(disease), !is.na(cp))

tab_cp <- table(heart_cp$disease, heart_cp$cp)
tab_cp
chisq.test(tab_cp)

#disease vs. sex
heart_sex <- heart %>% drop_na(disease, sex)
tab_sex <- table(heart_sex$disease, heart_sex$sex)
print(tab_sex)
chisq.test(tab_sex)

#disease vs. exang
heart_exang <- heart %>% drop_na(disease, exang)
tab_exang <- table(heart_exang$disease, heart_exang$exang)
print(tab_exang)
chisq.test(tab_exang)

#disease vs. thal
heart_thal <- heart %>% drop_na(disease, thal)
tab_thal <- table(heart_thal$disease, heart_thal$thal)
print(tab_thal)
chisq.test(tab_thal)

#clean data to create models 
heart_clean <- heart %>%
  drop_na(disease, sex, cp, fbs, exang, restecg, slope, ca, thal)

#Logistic Regression Models 

#Model 1:
mod1 <- glm(disease ~ cp, data = heart_clean, family = binomial)


#Model 1 : cp ONLY
pred_mod1 <- heart_clean %>%
  mutate(pred_prob = predict(mod1, type = "response")) %>%
  group_by(cp) %>%
  summarize(mean_prob = mean(pred_prob), .groups = "drop")

ggplot(pred_mod1, aes(x = cp, y = mean_prob)) +
  geom_col() +
  labs(
    title = "Model 1: Mean Predicted Probability by Chest Pain Type",
    x = "Chest Pain Type",
    y = "Mean Predicted Probability"
  ) +
  theme_minimal()
            
#Model 2 : cp + sex 

mod2 <- glm(disease ~ cp + sex, data = heart_clean, family = binomial)

pred_mod2 <- heart_clean %>%
  mutate(pred_prob = predict(mod2, type = "response")) %>%
  group_by(cp, sex) %>%
  summarize(mean_prob = mean(pred_prob), .groups = "drop")

ggplot(pred_mod2, aes(x = cp, y = mean_prob, fill = sex)) +
  geom_col(position = "dodge") +
  labs(
    title = "Model 2: Mean Predicted Probability by CP and Sex",
    x = "Chest Pain Type",
    y = "Mean Predicted Probability",
    fill = "Sex"
  ) +
  theme_minimal()
            
#Model 3 : cp + sex + fbs 

mod3 <- glm(disease ~ cp + sex + fbs, data = heart_clean, family = binomial)

pred_mod3 <- heart_clean %>%
  mutate(pred_prob = predict(mod3, type = "response")) %>%
  group_by(cp, sex, fbs) %>%
  summarize(mean_prob = mean(pred_prob), .groups = "drop")

ggplot(pred_mod3, aes(x = cp, y = mean_prob, fill = fbs)) +
  geom_col(position = "dodge") +
  facet_wrap(~ sex) +
  labs(
    title = "Model 3: Mean Predicted Probability by CP, Sex, and FBS",
    x = "Chest Pain Type",
    y = "Mean Predicted Probability",
    fill = "FBS"
  ) +
  theme_minimal()

#Model 4 : all categorical variables 

mod4 <- glm(disease ~ cp + sex + fbs + exang + restecg + slope + ca + thal, 
            data = heart_clean, family = binomial)

pred_mod4 <- heart_clean %>%
  mutate(pred_prob = predict(mod4, type = "response")) %>%
  group_by(cp, thal) %>%
  summarize(mean_prob = mean(pred_prob), .groups = "drop")

ggplot(pred_mod4, aes(x = cp, y = mean_prob, fill = thal)) +
  geom_col(position = "dodge") +
  labs(
    title = "Model 4: Mean Predicted Probability by CP and Thal (Full Model)",
    x = "Chest Pain Type",
    y = "Mean Predicted Probability",
    fill = "Thal"
  ) +
  theme_minimal()

tidy(mod1, exponentiate = TRUE, conf.int = TRUE)
tidy(mod2, exponentiate = TRUE, conf.int = TRUE)
tidy(mod3, exponentiate = TRUE, conf.int = TRUE)
tidy(mod4, exponentiate = TRUE, conf.int = TRUE)

#comparing AIC
model_compare <- tibble(
  Model = c("Model 1: CP","Model 2: CP + Sex",
            "Model 3: CP + Sex + FBS","Model 4: Full Model"),
  AIC = c(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4))
)

print(model_compare)

#ratio test 
anova(mod1, mod2, test="Chisq")
anova(mod2, mod3, test="Chisq")
anova(mod3, mod4, test="Chisq")


# fitted values and Pearson residuals to the analysis dataset
heart_diag <- heart_clean %>%
  mutate(
    fitted_mod4        = fitted(mod4),                     # predicted probability of Disease
    pearson_resid_mod4 = residuals(mod4, type = "pearson") # Pearson residuals
  )

# 1) Residuals vs fitted plot (overall model fit)
ggplot(heart_diag, aes(x = fitted_mod4, y = pearson_resid_mod4)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals vs Fitted Values (Model 4)",
    x = "Fitted Probability of Disease",
    y = "Pearson Residuals"
  ) +
  theme_minimal()

# 2) Pearson residuals by chest pain type (cp)
ggplot(heart_diag, aes(x = cp, y = pearson_resid_mod4)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals by Chest Pain Type (Model 4)",
    x = "Chest Pain Type",
    y = "Pearson Residuals"
  ) +
  theme_minimal()

# 3) Pearson residuals by sex
ggplot(heart_diag, aes(x = sex, y = pearson_resid_mod4)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pearson Residuals by Sex (Model 4)",
    x = "Sex",
    y = "Pearson Residuals"
  ) +
  theme_minimal()

# 4) Look at largest residuals (where model fits worst)
heart_diag %>%
  arrange(desc(abs(pearson_resid_mod4))) %>%
  select(disease, cp, sex, fbs, exang, restecg, slope, ca, thal,
         fitted_mod4, pearson_resid_mod4) %>%
  head(10)