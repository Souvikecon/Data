path="D:/JSY/Data" #set path here.
setwd(path) ## setting working directory to path

library(haven)
library(ggplot2)
library(grf)
# library(openxlsx)
library(tidyverse)  # ggplot(), %>%, mutate(), and friends
# library(broom)  # Convert models to data frames
library(modelsummary)  # Make side-by-side regression tables

set.seed(4596)

keep=c("clusterid","birthweight","JSY","BPL","SC","ST","OBC","Hindu",
       "Muslim","age_mother","education_mother","birth_order",
       "wealth_index","rural","mother_height","female_child",
       "deadchild","pregnancy_complications","tetanus_injections",
       "iron_tablets", "antenatal_care","antenatal_visits",
       "prenatal_care_person","registered_card","female_head",
       "age_household_head","LPS")

df=read_dta("JSY_filtered.dta")%>% select(all_of(keep)) %>% na.omit()

X=df[,4:27] %>% as.matrix()
Y=log(df$birthweight) %>% as.matrix()
W=df$JSY %>% as.matrix()

cf.raw = causal_forest(X , Y , W)
varimp.raw = variable_importance(cf.raw)
selected.idx = which(varimp.raw > mean(varimp.raw))

# varimp.raw_df= data.frame(varimp.raw)
# varimp.raw_df$variables= names(df[,4:27])

X_cf=X[,selected.idx]
cf = causal_forest(X_cf, Y , W, tune.parameters = "all")

tau.hat = predict(cf)$predictions
tau=predict(cf,estimate.variance = T)
tau= tau %>% mutate(sd=sqrt(variance.estimates)) %>%
  mutate(lower_ci=predictions-1.96*sd,upper_ci=predictions+1.96*sd)
df$predictions=tau$predictions

ATE = average_treatment_effect(cf,target.sample="all")
ATT= average_treatment_effect(cf,target.sample="treated")

varimp = variable_importance(cf)
ranked.vars <- order(varimp, decreasing = TRUE)
# Top 5 variables according to this measure
colnames(X_cf)[ranked.vars]

# best linear projection
blp.rankedvars=best_linear_projection(cf, X[,ranked.vars])
blp.rankedvars_coef_df <- blp.rankedvars[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(blp.rankedvars)) # BLP results as dataframe

varimp_df= data.frame(varimp)
varimp_df$variables= names(df[,4:27])
varimp_df_sorted= arrange(varimp_df,desc(varimp))

p <- ggplot(varimp_df,aes(x = reorder(variables,+varimp), y = varimp))+geom_col(width = 0.7)
p+coord_flip()

hist(tau.hat) # Distribution of CATE
hist(e.hat <- cf$W.hat)

test_calibration(cf) # Run best linear predictor analysis

df1= data.frame(df$antenatal_visits,df$mother_height,tau) %>% rename()
df1 %>% sample_frac(0.5,replace = T) %>% ggplot(aes(x = df.antenatal_visits, y = predictions)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.1)

df1 %>% sample_frac(0.5,replace = T) %>% ggplot(aes(x = df.mother_height, y = predictions)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.1)

IPW <- ifelse(W == 1, 1 / e.hat, 1 / (1 - e.hat))

plot.df <- data.frame(value = as.vector(X),
                      variable = colnames(X),
                      W = as.factor(W),
                      IPW = IPW)

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(wealth_index))) + 
  geom_boxplot()+
  geom_smooth(method="loess",aes(group=1),se=F)


df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(education_mother))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% filter(antenatal_visits<=10) %>%
  ggplot(aes(y =predictions, x = factor(antenatal_visits))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))
# method = "lm", se=TRUE, aes(group=1)

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(mother_height))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(tetanus_injections))) + 
  geom_boxplot()+
  geom_smooth(method="lm",aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(birth_order))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(age_household_head))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(y =predictions, x = factor(age_mother))) + 
  geom_boxplot()+
  geom_smooth(aes(group=1))

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(x = wealth_index , y = education_mother, fill = predictions)) +
  scale_fill_gradient(high = "red", low = "white")+
  geom_tile() ## heatmap WI & education of mother

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(x = age_mother, y = education_mother, fill = predictions)) +
  scale_fill_gradient(high = "red", low = "white")+
  geom_tile() ## heatmap WI & education of mother

df %>% sample_frac(0.5,replace = T) %>% filter(antenatal_visits<=10) %>%
  ggplot(aes(x = wealth_index , y = antenatal_visits, fill = predictions)) +
  scale_fill_gradient(high = "red", low = "white")+
  geom_tile() ## heatmap WI & antenatal visits

df %>% sample_frac(0.5,replace = T) %>% 
  ggplot(aes(x = wealth_index , y = tetanus_injections, fill = predictions)) +
  scale_fill_gradient(high = "red", low = "white")+
  geom_tile() ## heatmap WI & tetanus injections
