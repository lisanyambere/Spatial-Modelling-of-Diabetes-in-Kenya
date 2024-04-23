#SPSS package for importing data 
library(haven)
#import data
Diabetes_2015_data<-read_sav("C:/Users/user/Desktop/Diabetes_2015_data.sav")

#Exploring data
head(Diabetes_2015_data)
tail(Diabetes_2015_data)
#packages
library(tidyverse)
library(dplyr)
library(magrittr)#load to use forward pipes i.e %>%
library(janitor)
#variable types
names(Diabetes_2015_data)
glimpse(Diabetes_2015_data)

#Renaming variables of interest that are coded

Diabetes_2015_data_corrected <- Diabetes_2015_data %>%
  rename(P_id = PID, Sex = C1, Height = M11, Weight = M12, Cholestrol = B8, Region = X1, Obesity_status = H20h, Physical_activity = H20g, Residential_area = X2a,
         Age_range = agerange, Fasting_blood_sugar = B5) %>%
  select(P_id, Age_range, Sex, Height, Weight, Cholestrol, Region, Obesity_status, Physical_activity,
         Residential_area, Fasting_blood_sugar) %>%
  filter(complete.cases(.)) #removing missing values from the data

#BMI Function
func_BMI<-function(Height,Weight){
  Body_Mass_Index<-(Weight/Height^2)*100
  return(Body_Mass_Index)
}
func_BMI(Diabetes_2015_data_corrected$Weight,Diabetes_2015_data_corrected$Height)

#summary statistics the variable Age,Diabetes status,Sex,Height
 summary(Diabetes_2015_data_corrected$Age_range)
 summary(Diabetes_2015_data_corrected$Sex)
 unique(Diabetes_2015_data_corrected$Sex)#possible values of sex variable
 summary(Diabetes_2015_data_corrected$Height)
 summary(Diabetes_2015_data_corrected$Weight)
 summary(Diabetes_2015_data_corrected$Cholestrol)
 summary(Diabetes_2015_data_corrected$Fasting_blood_sugar)
#count missing data in each column
sum(is.na(Diabetes_2015_data_corrected$Age_range))
sum(is.na(Diabetes_2015_data_corrected$Sex))
sum(is.na(Diabetes_2015_data_corrected$Height))
sum(is.na(Diabetes_2015_data_corrected$Weight))
sum(is.na(Diabetes_2015_data_corrected$Cholestrol))
sum(is.na(Diabetes_2015_data_corrected$Region))
sum(is.na(Diabetes_2015_data_corrected$Obesity_status))
sum(is.na(Diabetes_2015_data_corrected$Physical_activity))
sum(is.na(Diabetes_2015_data_corrected$Residential_area))
sum(is.na(Diabetes_2015_data_corrected$Age_range))
sum(is.na(Diabetes_2015_data_corrected$Fasting_blood_sugar))
#Replacing missing values and adding BMI variable
Diabetes_2015_data_corrected1<-Diabetes_2015_data_corrected 
 
# Removing duplicated rows
Diabetes_2015_data_corrected1d <- unique(Diabetes_2015_data_corrected1) 
  
#keep only unique rows
Diabetes_2015_data_corrected1d %>%
  unique() %>%
view()
 
#Re-coding coded variables

Diabetes_2015_data_recoded<-Diabetes_2015_data_corrected1d %>%
  select(P_id,Age_range,Sex,Height,Weight,Cholestrol,Region,Obesity_status,Physical_activity,
         Residential_area,Fasting_blood_sugar) %>%
  mutate(Residential_area=as.factor(Residential_area),Residential_area=recode(Residential_area,'1'="Rural",'2'="Urban"),Physical_activity=as.factor(Physical_activity),Physical_activity=recode(Physical_activity,'1'="Yes",'2'="No"),Sex=as.factor(Sex),Age_range=as.factor(Age_range),Obesity_status=as.factor(Obesity_status),Obesity_status=recode(Obesity_status,'1'="Yes",'2'="No"),Region=as.character(Region),Region=recode(Region,'1'="Baringo",'2'="Bomet",
                                                   '3'="Bungoma",'4'="Busia",'5'="Elgeyo Marakwet",'6'="Embu",'7'="Garissa",
                                                   '8'="Homabay",'9'="Isiolo",'10'="Kajiado",'11'="Kakamega",'12'="Kericho",
                                                   '13'="Kiambu",'14'="Kilifi",'15'="Kirinyaga",'16'="Kisii",'17'="Kiambu",'18'="Kitui",
                                                   '19'="Kwale",'20'="Laikipia",'21'="Lamu",'22'="Machakos",'23'="Makueni",'24'="Mandera",
                                                   '25'="Marsabit",'26'="Meru",'27'="Migori",'28'="Mombasa",'29'="Murang'a",'30'="Nairobi",
                                                   '31'="Nakuru",'32'="Nandi",'33'="Narok",'34'="Nyamira",'35'="Nyandarua",'36'="Nyeri",'37'="Samburu",
                                                   '38'="Siaya",'39'="Taita Taveta",'40'="Tana River",'41'="Tharaka-Nithi",'42'="Trans-Nzoia",'43'="Turkana",
                                                   '44'="Uasin Gishu",'45'="Vihiga",'46'="Wajir",'47'="West Pokot")) %>%
 

 view()

#Body mass index Classification in terms of overweight,Normal weight and underweight
#Fasting blood sugar classification in terms of normal and diabetic
Diabetes_dataset <- Diabetes_2015_data_recoded %>%
  mutate(
    BMI = Weight / (Height / 100)^2,
    Weight_Category = ifelse(BMI >= 25, "Overweight", 
                             ifelse(BMI >= 18.5, "Normal weight", "Underweight")),
    Diabetes_status = ifelse(Fasting_blood_sugar <= 5.6, "No", "Yes")
  )%>%
  view()

#our data(cleaned)
nrow(Diabetes_dataset[ complete.cases(Diabetes_dataset) != TRUE, ]) 

#Remove duplicates
Diabetes_2015_data_cleaned<-filter(Diabetes_dataset[ duplicated(Diabetes_dataset) != TRUE, ])
##Final data set for analysis
view(Diabetes_2015_data_cleaned)
#import Kenya shape file
library(sf)
library(ggplot2)
library(magrittr)
KenyaSHP<-read_sf("C:/Users/user/Downloads/kenya-county-20240311T064855Z-001/kenya-county/Shapefile/ke_county.shp")
view(KenyaSHP)
plot(KenyaSHP)
#merge shape file with data
### Method 2
names(KenyaSHP)
### To easily view the shape file in RStudio View pane, you can drop the geometry column and view the rest of the data.
KenyaSHP<- KenyaSHP %>%
  select(-c("pop 2009","lf_endemic","country","counts","var1_class" ,"hiv_class", "hiv_his_cl","cuid"))
### View(Kenyan %>% st_drop_geometry())
names(KenyaSHP$gid)<-('ID')
#inspect the rows
#print(KenyaSHP[6:9], n = 3)
#inspect columns
colnames(KenyaSHP)
#inspect class
class(KenyaSHP)
#look at variable types
glimpse(KenyaSHP)
# view geometry column
KenyaSHP_geometry <- st_geometry(KenyaSHP)

### View one geometry entry
#KenyaSHP_geometry[[1]]
# geometry columns have their own class
class(KenyaSHP_geometry) #sfc, the list-column with the geometries for each feature


class(KenyaSHP_geometry[[1]]) #sfg, the feature geometry of an individual simple feature
#change the projection of the shapefiles
### This line is not necessary since the shapefile is already in the WGS 84 projection.

KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(KenyaSHP)

#load the data to be merged with the shape file

steps_df <- Diabetes_2015_data_cleaned
view(steps_df)
#Clean the data, so that the counties match those in the shapefile.

### Inspect the county names of the disability data
counties_steps_df <- unique(steps_df$Region)

### Inspect the county names of the shape file
counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  select(county) %>% 
  pull() %>%
  unique()

### Convert the disability county names to title case
steps_df <- steps_df %>% 
  ungroup() %>% 
  mutate(Region = tools::toTitleCase(tolower(Region)))

### Inspect the county names of the disability data again 
counties_steps_df <- unique(steps_df$Region)

### Inspect the county names that are different in each of the datasets
unique(steps_df$Region)[which(!unique(steps_df$Region) %in% counties_KenyaSHP)]


### Clean the county names so that they match in both datasets
steps_df <- steps_df %>% 
  mutate(Region = ifelse(Region == "Elgeyo Marakwet", "Elgeyo-Marakwet",
                         ifelse(Region == "Trans-Nzoia", "Trans Nzoia",
                                ifelse(Region == "Tharaka-Nithi", "Tharaka-nithi",
                                       ifelse(Region == "Homabay" , "Homa Bay", Region)))))


### Inspect the county names again to ensure that they now match.
unique(steps_df$Region)[which(!unique(steps_df$Region) %in% counties_KenyaSHP)]

names(steps_df)
### unique(Disability_df2$County)[which(!unique(Disability_df2$County) %in% counties_KenyaSHP)]

### counties_KenyaSHP[which(!counties_KenyaSHP  %in% unique(Disability_df2$County))]

#Join the shapefile and the data
### Rename the COUNTY variable, to match the variable name in the shapefile data
steps_df <- steps_df %>% 
  rename( county= Region)

### Ensure that there are no leading or trailing spaces in the county variable
KenyaSHP$county <- trimws(KenyaSHP$county)
steps_df$county <- trimws(steps_df$county)
names(KenyaSHP)
names(steps_df)
### Merge the data
merged_df <- inner_join(steps_df,KenyaSHP ,by = "county")

dim(merged_df)
### Sort the data so that the County variable appears first
merged_df <- merged_df %>% 
  select(county, everything())

  dim(merged_df)
  
  names(merged_df)

########Convering into factors
merged_df$Age_range<-as.factor(merged_df$Age_range) 
merged_df$Age_range
str(merged_df$Age_range)
merged_df$Sex<-as.factor(merged_df$Sex)
str(merged_df$Sex)
merged_df$Obesity_status<-as.factor(merged_df$Obesity_status) 
str(merged_df$Obesity_status) 
merged_df$Physical_activity<-as.factor(merged_df$Physical_activity) 
str(merged_df$Physical_activity)
merged_df$Residential_area<-as.factor(merged_df$Residential_area)
str(merged_df$Residential_area)
merged_df$Weight_Category<-as.factor(merged_df$Weight_Category)
str(merged_df$Weight_Category)
merged_df$Diabetes_status<-as.factor(merged_df$Diabetes_status)
str(merged_df$Diabetes_status)
str(merged_df)
merged_df$BMI<-as.numeric(merged_df$BMI)
str(merged_df$BMI)
class(merged_df$geometry)
#Fitting Generalized linear mixed models
library(MatrixModels)
library(Matrix)
library(spdep)
library(sp)
library(dplyr)
library(tidyr)
library(lme4)
library(broom.mixed)
str(merged_df)
names(merged_df)

#structured_model <- glm(Diabetes_status ~ Residential_area + Age_range + Sex + Weight_Category + Obesity_status + Physical_activity, 
 #                       data = merged_df, family = binomial(link = "logit"))

#structured_model
#structured_model <-glmer(Diabetes_status ~ Residential_area + Age_range + Sex + Weight_Category + Obesity_status
#                            + Physical_activity + (1 | county),family = 'binomial' ,
#                      data = merged_df)
# Load the required packages
library(lme4)
library(Matrix)
control_params <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e7))
# Fit the model
structured_model <- glmer(Diabetes_status ~ Residential_area + Cholestrol + Age_range + Sex + Weight_Category + BMI + Obesity_status + Physical_activity + (1 | county), 
                          family = binomial(link = "logit"), 
                          data = merged_df,control = control_params)

structured_model
unstructured_model <- glmer(Diabetes_status ~ Residential_area + Cholestrol + Age_range + Sex + Weight_Category + BMI + Obesity_status + Physical_activity + (1 | P_id), 
                          family = binomial(link = "logit"), 
                          data = merged_df,control = control_params)

unstructured_model

both_model <- glmer(Diabetes_status ~ Residential_area + Cholestrol + Age_range + Sex + Weight_Category + BMI + Obesity_status + Physical_activity + (1 | county) + (1 | P_id), 
                            family = binomial(link = "logit"), 
                            data = merged_df,control = control_params)

both_model

################################################################################################################################################
library(magrittr)
library(sjPlot)
library(tidybayes)
library(sjlabelled)
library(gtsummary)
library(gt)
# Convert models to tbl_regression objects
# format results into data frame with global p-values

tbl_structured_model<- structured_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
    bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
  tbl_structured_model
# Get coefficients of the structured_model
coefficients_model1 <- coef(structured_model)

# View the coefficients
print(coefficients_model1)

tbl_unstructured_model<- unstructured_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_unstructured_model
# Get coefficients of the structured_model
coefficients_model2 <- coef(unstructured_model)

# View the coefficients
print(coefficients_model2)

tbl_both_model<- both_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_both_model
# Get coefficients of the structured_model
coefficients_model3 <- coef(both_model)

# View the coefficients
print(coefficients_model3)
# Add model names
tbl_structured_model <- modify_caption(tbl_structured_model, 
                                       caption = "Model: Model 1")
tbl_unstructured_model <- modify_caption(tbl_unstructured_model, 
                                         caption = "Model: Model 2")
tbl_both_model <- modify_caption(tbl_both_model, 
                              caption = "Model: Model 3")
#

# Combine tables into a single gtsummary table
combined_tbl <- tbl_merge(
  tbls = list(tbl_structured_model, tbl_unstructured_model, tbl_both_model),
  tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**")
)
# Print the combined table
combined_tbl
gtsave(combined_tbl,"combined_tbl.docx")
#save the output of the models
library(webshot2)
combined_tbl <- as_gt(combined_tbl)
gtsave(combined_tbl, file = "combined_table.pdf")

#####structured model
tbl_structured_model1 <- modify_caption(tbl_structured_model, 
                                    caption = "Model: structured_model")
print(tbl_structured_model1)
##cross tabulation
library(tidyverse)
library(gt)
library(gtsummary)

fn_subtable <- function(data, main, sub){
  data %>% 
    dplyr::select({{main}},{{sub}}) %>% 
    gtsummary::tbl_summary(
      by = {{sub}}, 
      digits = list(dplyr::everything() ~ c(2, 0))) %>%
    gtsummary::add_p()
  
}

fn_table3 <- function(data, main_var, sub_vars){
  
  main_var <- rlang::enexpr(main_var)
  sub_vars_expr <- rlang::enexpr(sub_vars)         # 1. Capture `list(...)` call as expression
  sub_vars_args <- rlang::call_args(sub_vars_expr) # 2. Pull out the arguments (they're now also exprs)
  sub_vars_fn   <- rlang::call_fn(sub_vars_expr)   # 3. Pull out the fn call
  # 4. Evaluate the fn with expr-ed arguments (this becomes `list( expr(agegp), expr(alcgp) )` )
  sub_vars_reconstructed <- rlang::exec(sub_vars_fn, !!!sub_vars_args)
  
  # --- sub_vars replaced with sub_vars_reconstructed from here onwards ---
  
  t0 <- data %>% 
    dplyr::select({{main_var}}) %>% 
    gtsummary::tbl_summary(statistic = gtsummary::all_categorical() ~ "{p}% ({n})",
                           digits = list(dplyr::everything() ~ c(2, 0))) %>%
    gtsummary::modify_header(label ~ "") %>%
    gtsummary::bold_labels()
  
  sub_tables <- purrr::map(sub_vars_reconstructed, ~fn_subtable(data = data, main = main_var, sub = .x))
  
  tbls <-  c(list(t0), sub_tables) %>% 
    gtsummary::tbl_merge(tab_spanner = c("**Total**", paste0("**",sub_vars_reconstructed,"**"))) %>%
    gtsummary::as_gt() %>% 
    gt::tab_source_note(gt::md("*Cross tabulations*"))
  
  tbls
  
}

#pvalue
Table_D<-fn_table3(merged_df,Diabetes_status,list(Residential_area ,Age_range,Sex,Obesity_status ,Weight_Category,
                                        Physical_activity))
Table_D



gtsave(Table_D, file = "C:/Users/user/Desktop/Table_D.docx")


  
##### table1
library(table1)
Table_summary<-table1(~ Residential_area + Age_range + Sex + Cholestrol +Obesity_status + Weight_Category+ 
         Physical_activity |Diabetes_status, data=merged_df, overall=F,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times")
Table_summary
gtsave(Table_summary, file = "C:/Users/user/Desktop/Table_summary.docx")

#table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid Rtable1-shade Rtable1-times")
#table1(~ age + race + married + nodegree + re74 + re75 + re78 | treat,
#data=lalonde, overall=F, extra.col=list(`P-value`=pvalue))
####################################################################################################################
#INLA
library(INLA)
library(ggplot2)
library(spdep)
library(DClusterm)
library(sp)

sp_polygons <- st_as_sf(merged_df)
spdf_polygons <- as(sp_polygons, "Spatial")

diab.nb <- poly2nb(spdf_polygons)

# Bayesian analysis and prediction using INLA
## create adjacency matrix
class(merged_df)
diab.nb <- poly2nb(merged_df)
# Create sparse adjacency matrix
diab.mat <- as(nb2mat(diab.nb, style = "B"), "Matrix")

# fit icar model
diab.icar<-inla(Diabetes_status ~ Residential_area + Age_range + Sex + Weight_Category + Obesity_status
                + Physical_activity + (1 | county), 
                family = 'binomial',data = as.data.frame(merged_df),control.predictor = list(compute = TRUE),
                control.compute = list(dic = TRUE, waic = TRUE))
summary(diab.icar)


diab.icar <- inla(Diabetes_status ~ Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + Harmful_alcohol_intake + 
                    High_sugar_intake + Physical_inactivity + Tobacco_use + Bad_fat_intake + f(COUNTY, model = "besag", graph = diab.mat ), 
                  family = "binomial", data = as.data.frame(merged_df),
                  control.predictor = list(compute = TRUE),
                  control.compute = list(dic = TRUE, waic = TRUE))

summary(diab.icar)

# fit bym model
diab.bym <- inla(Diabetes ~ Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + Harmful_alcohol_intake + 
                   High_sugar_intake + Physical_inactivity + Tobacco_use + Bad_fat_intake + f(COUNTY, model = "bym", graph = diab.mat ), 
                 family = "binomial", data = as.data.frame(merged_df),
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE))

summary(diab.bym)

#fit leroux model
## create a matrix first
ICARmatrix <- Diagonal(nrow(diab.mat), apply(diab.mat, 1, sum)) - diab.mat
Cmatrix <- Diagonal(nrow(merged_df), 1) -  ICARmatrix
### check; max eigen value should be 1
max(eigen(Cmatrix)$values)

diab.ler <- inla(Diabetes ~ Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + Harmful_alcohol_intake + 
                   High_sugar_intake + Physical_inactivity + Tobacco_use + Bad_fat_intake + f(COUNTY, model = "generic1", Cmatrix = Cmatrix ), 
                 family = "binomial", data = as.data.frame(merged_df),
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE))

summary(diab.ler)

# fit spatial lag model
##model definition
#X
mmatrix <- model.matrix(Diabetes ~ Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + Harmful_alcohol_intake + 
                          High_sugar_intake + Physical_inactivity + Tobacco_use + Bad_fat_intake, merged_df)
#W
W <- as(nb2mat(diab.nb, style = "W"), "Matrix")
#Q
Q.beta = Diagonal(n = ncol(mmatrix), x = 0.001)
#Range of rho
rho.min<- -1
rho.max<- 1
#Arguments for 'slm'
args.slm = list(
  rho.min = rho.min ,
  rho.max = rho.max,
  W = W,
  X = mmatrix,
  Q.beta = Q.beta
)
#Prior on rho
hyper.slm = list(
  prec = list(
    prior = "loggamma", param = c(0.01, 0.01)),
  rho = list(initial=0, prior = "logitbeta", param = c(1,1))
)
## model fitting
#SLM model
diab.slm <- inla( Diabetes ~ -1 +
                    f(COUNTY, model = "slm", args.slm = args.slm, hyper = hyper.slm),
                  data = as.data.frame(merged_df), family = "binomial",
                  control.predictor = list(compute = TRUE),
                  control.compute = list(dic = TRUE, waic = TRUE)
)
## Warning in inla.model.properties.generic(inla.trim.family(model), (mm[names(mm) == : Model 'slm' in section 'latent' is marked as 'experimental'; changes may appear at any time.
##   Use this model with extra care!!! Further warnings are disabled.
summary(diab.slm)
##estimates of the coefficients
round(m.slm$summary.random$ID[47:48,], 4)
##spatial autocorrelation
marg.rho.internal <- m.slm$marginals.hyperpar[["Rho for county"]]
marg.rho <- inla.tmarginal( function(x) {
  rho.min + x * (rho.max - rho.min)
}, marg.rho.internal)

inla.zmarginal(marg.rho, FALSE)
plot(marg.rho, type = "l", main = "Spatial autocorrelation")

##model selection based on the criterions

##summary of the results
##plotting posterior means
merged_df$ICAR <- diab.icar$summary.fitted.values[, "mean"]
merged_df$BYM <- diab.bym$summary.fitted.values[, "mean"]
merged_df$LEROUX <- diab.ler$summary.fitted.values[, "mean"]
merged_df$SLM <- diab.slm$summary.fitted.values[, "mean"]
spplot(merged_df$geometry, 
       c( "ICAR", "BYM", "LEROUX", "SLM"),
       col.regions = rev(magma(16))
)













