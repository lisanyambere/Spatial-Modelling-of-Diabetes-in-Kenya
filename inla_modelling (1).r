library(INLA)
library(ggplot2)
library(spdep)
library(DClusterm)
library(sp)
library(sf)
# convert data frame to a sf object
merged_sf<- st_as_sf(merged_df, wkt = 'geometry')
# omit the missing
merged_sf <- na.omit(merged_sf)
#convert diabetes and ID to numeric
merged_sf$Diabetes<- as.numeric(as.character(merged_sf$Diabetes))
merged_sf$ID<- as.numeric(merged_sf$ID)
merged_sf<- merged_sf[,-1]
# convert sf object to spatial object
#merged_sp<- as(merged_sf, "Spatial")

table(sf:: st_is_valid(merged_sf))
# Bayesian analysis and prediction using INLA
## create adjacency matrix
diab.nb <- poly2nb(sf::st_make_valid(merged_sf))

# Create sparse adjacency matrix
diab.mat <- as(nb2mat(diab.nb, style = "B"), "Matrix")


# fit icar model
diab.icar <- inla(Diabetes ~   Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + 
                      Physical_inactivity + f(ID, model = "besag", graph = diab.mat ), 
                     family = "binomial", data = as.data.frame(merged_sf),
                     control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE))
#ICAR <- diab.icar$summary.fitted.values[, "mean"]
#ICAR
  summary(diab.icar)

# fit bym model
  diab.bym <- inla(Diabetes ~   Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + 
                     Physical_inactivity + f(ID, model = "bym", graph = diab.mat ), 
                     family = "binomial", data = as.data.frame(merged_sf),
                     control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE))

  summary(diab.bym)

#fit leroux model
## create a matrix first
ICARmatrix <- Diagonal(nrow(diab.mat), apply(diab.mat, 1, sum)) - diab.mat
Cmatrix <- Diagonal(nrow(merged_sf), 1) -  ICARmatrix
### check; max eigen value should be 1
max(eigen(Cmatrix)$values)

diab.ler <- inla(Diabetes ~   Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + 
                   Physical_inactivity+ f(ID, model = "generic1", Cmatrix = Cmatrix ), 
                     family = "binomial", data = as.data.frame(merged_sf),
                     control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE))

  summary(diab.ler)

# fit spatial lag model
##model definition
#X
mmatrix <- model.matrix(Diabetes ~   Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + 
                          Physical_inactivity, merged_sf)
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
     f(ID, model = "slm", args.slm = args.slm, hyper = hyper.slm),
   data = as.data.frame(merged_sf), family = "binomial",
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
merged_sf$BYM_mean <- diab.bym$summary.fitted.values[, "mean"]
merged_sf$BYM_median <- diab.bym$summary.fitted.values[, "0.5quant"]
merged_sf$BYM_2.5 <- diab.bym$summary.fitted.values[, "0.025quant "]
merged_sf$BYM_97.5 <- diab.bym$summary.fitted.values[, "0.975quant"]

spplot(merged_sf, 
  c( "BYM_mean", "BYM_median", "BYM_2.5", "BYM_97.5"),
  col.regions = rev(magma(16))
)
# save the merged_sf to a shapefile
#convert it to sf object
sf_obj<- st_as_sf(merged_sf)
# now, save the sf object to a shapefile
st_write(sf_obj, dsn = "C:/Users/manub/OneDrive/Desktop/spatial_model", layer = "inla.results", driver = "ESRI shapefile")
# save merged_sf as csv file
inla.res<-st_drop_geometry(merged_sf)
write.csv(inla.res, "inla_res.csv")


# aggregate the posterior estimates by ID
aggregated_data<- aggregate(cbind(BYM_mean,BYM_median,BYM_97.5 , BYM_2.5) ~ ID,
                            data = inla_res, FUN = mean )
# merging the aggregated data and the shapefile
library(tidyverse) 
library(sf)
#KenyaSHP <- read_sf("C:/Users/manub/OneDrive/Desktop/Analytics/GIS/Data/kenya-county/Shapefile/ke_county.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)
KenyaSHP <- read_sf("C:/Users/manub/OneDrive/Desktop/Analytics/GIS/shapefiles/ken_admbnda_adm1_iebc_20191031.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)
#remove desired columns 

KenyaSHP<- KenyaSHP%>%
  select(-c(  "ADM1_PCODE","ADM1_REF" ,"ADM1ALT1EN","ADM1ALT2EN", "ADM0_EN",
              "ADM0_PCODE","date"  , "validOn"  ,"validTo","Shape_Leng" , "Shape_Area" ))

# rename column county
KenyaSHP<- KenyaSHP%>%
  rename(COUNTY = ADM1_EN)
# create column ID 
ID_column<- 1:47

KenyaSHP <- KenyaSHP %>%
  mutate(ID = ID_column)
KenyaSHP<- KenyaSHP[,-1]
### Merge the data
inla.res <- left_join(KenyaSHP, inla_res, by = "ID")

### Sort the data so that the County variable appears first
inla.res <- inla.res %>% 
  select(ID, everything())
# impute the missing values with the mean
missing_vars<- c( "BYM_mean"  , "BYM_median", "BYM_97.5" ,  "BYM_2.5"   )
inla.res<- inla.res %>%
  mutate_at(vars(missing_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))
# create the model adequacy table

Model<- c('ICAR', 'BYM', 'GENERIC1', 'SLM')
AIC<- c(743.17, 742.87, 743.17, 816.37)
WAIC<- c(743.55, 743.33, 743.55, 814.77)
adq_tabl<- data.frame(Model, AIC, WAIC)
adq_tabl