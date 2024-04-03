library(haven)
library(psych)
library(mice)
library(VIM)
library(dplyr)

path<- "O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Data"
setwd("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Consequences_child") 

D1<-read_spss(paste(path,"CHILD-ALLGENERALDATA_24102022.sav",sep="/"))
D1 <- D1[is.na(D1$RemoveData),]
D1$age_M <- D1$AGE_M_v2
D1$income <- as.factor(ifelse(D1$INCOME<7, 1, ifelse(D1$INCOME>6 & D1$INCOME<11, 2, ifelse(D1$INCOME>10, 3, NA))))
D1$income <- ordered(D1$income, levels= c(1,2,3))
D1$ethniM <- as.factor(ifelse(D1$ETHNMv2==1, 0, ifelse(is.na(D1$ETHNMv2), NA, 1)))
D1$ethniC <- as.factor(ifelse(D1$ETHNINFv2==1, 0, ifelse(is.na(D1$ETHNINFv2), NA, 1)))
D1$parity <- as.factor(ifelse(D1$PARITY==0, 0, ifelse(D1$PARITY==1, 1, ifelse(D1$PARITY==2, 2, ifelse(D1$PARITY<=3, 3, NA))))) # 0, 1, 2, >=3
D1$maritalstatus <- as.factor(ifelse(D1$MAR_ST2==999, NA, D1$MAR_ST2)) #"Married", "Cohabiting", "Single"
D1$educM <- as.factor(ifelse(D1$EDUCM<3, 1, ifelse(D1$EDUCM==3, 2, ifelse(D1$EDUCM==4, 3, ifelse(D1$EDUCM==5, 4, NA)))))
D1$educM <- ordered(D1$educM, levels= c(1,2,3,4)) # Low, Midlow, Midhigh, High
D1$BMI_M <- D1$BMI_0
D1$gest_age <- D1$GESTBIR
D1$sex <- as.factor(D1$GENDER)
D1$birthweight <- D1$WEIGHT
D1$apgar <- D1$APGAR5

D2<-read_spss(paste("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Data\\GR1001-A_22112016.sav",sep="/"))
data <- merge(D1, D2, by.x = "IDM", by.y = "idm", all.x = T)
data$planned <- as.factor(ifelse(data$a0500101==1, 0, ifelse(data$a0500101==0, 1, NA))) # 0=planned, 1=unplanned
data$feelings <- as.factor(ifelse(data$planned==1 & is.na(data$a0500301), 9 , ifelse(data$planned==1, data$a0500301, ifelse(data$planned==0, 0 ,NA))))  # 0=planned, 1=pleased, 2=mixed intially, 3=mixed still, 4=unhappy, 9= unplanned but feelings unknown (n=184)
data$UP <- as.factor(ifelse(data$planned==1 & data$a0500301==1, 1, ifelse(data$planned==1 & data$a0500301==2, 2, 
                   ifelse(data$planned==1 & data$a0500301==3, 3, ifelse(data$planned==1 & data$a0500301==4, 3, 
                                                                       ifelse(data$planned==0, 0 ,NA))))))  # 0=planned, 1=pleased, 2=mixed intially, 3=mixed still/unhappy

D3<-read_spss(paste("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Data\\20210629_Clair-UsecontraceptivesLMP.sav",sep="/"))
data <- merge(data, D3, by = "IDM", all.x = T) # contraceptives
data$contraceptive_use <- as.factor(ifelse(is.na(data$contraceptives), 9, data$contraceptives)) # 0 no 1 yes 9 missing

D4<-read_spss(paste(path,"GR1001-D1-37_16072021.sav",sep="/"))
data <- merge(data, D4, by = "IDM", all.x = T )
data$sexpartners <- as.factor(data$d0500101) #0=1, 1=>1 partner

D5<-read_spss(paste(path,"GR1003-D2-20_01072012.sav",sep="/")) # ever depression, anxiety vignettes
data <- merge(data, D5, by = "IDM", all.x = T)
data$depression_everM <- as.factor(data$depression_ever)
data$anxiety_everM <- as.factor(data$anxiety_ever)

D6<-read_spss(paste(path,"GR1003-E_01072012.sav",sep="/")) # eating disorder
data <- merge(data, D6, by = "IDM", all.x = T)
data$ED_everM <- as.factor(data$ED_ever)

D7<-read_spss(paste(path,"GR1004-G_01072012.sav",sep="/")) # depression, anxiety vignettes
D7$depression_everF <- as.factor(D7$depression_ever)
D7$anxiety_everF <- as.factor(D7$anxiety_ever)
data <- merge(data, D7, by = "IDM", all.x = T)

D8<-read_spss(paste(path,"MATERNALFOLICACID_23062010.sav",sep="/"))  
data <- merge(data, D8, by = 'IDM', all.x = T)
data$folic_acid <- as.factor(ifelse(is.na(data$FOLIUM_VALIDATED), 9, data$FOLIUM_VALIDATED)) # 0=no, 1=in the first 10 wks, 2=prenatal, 9 missing

D9<-read_spss(paste(path,"ZIPCODEBIRTH_22112016.sav",sep="/"))  
data <- merge(data, D9, by = 'IDM', all.x = T)
data$zipcode <- as.factor(ifelse(table(data$ZIPCODE_BIRTH)[data$ZIPCODE_BIRTH] < 200 | data$ZIPCODE_BIRTH == "", NA, data$ZIPCODE_BIRTH))

D10<-read_spss(paste(path,"MATERNALSMOKING_22112016.sav",sep="/"))  
data <- merge(data, D10, by.x = 'IDM', by.y = 'idm', all.x = T)
data$smoking <- as.factor(data$SMOKE_ALL) # 1=never smoked during preg, 2=smoked untill preg was known, 3=continued smoking during preg 

D11<-read_spss(paste(path,"GEDRAGSGROEP_MaternalDrinking_22112016.sav",sep="/"))  
data <- merge(data, D11, by = 'IDM', all.x = T)
data$drinking <- as.factor(data$mdrink_updated) # 0=never drank during preg, 1=drank untill preg was known, 2=continued drinking occasionnaly, 3= continued drinking frequently (1 or more glass/week for at least 2 trimesters)

D12<-read_spss(paste(path,"GR1024-C_01072012.sav",sep="/"))
data <- merge(data, D12, by = 'IDC', all.x = T)
data$socialsupport <- data$ssli_m6

D13<-read_spss(paste(path,"GR1019-E_01072012.sav",sep="/"))
data <- merge(data, D13, by = 'IDC', all.x = T)
data$EPDS <- as.factor(data$EPDS2cat)

D14<-read_spss(paste(path,"20200111_GR1029_CBCL A1_1.5_incl_Tscores.sav",sep="/"))
data <- merge(data, D14, by = 'IDC', all.x = T)
data$int2 <- data$sum_int
data$ext2 <- data$sum_ext
data$age2 <- data$AGE18M/12

D15<-read_spss(paste(path,"CBCL_3_incl_Tscores__GR1065E2_GR1066A1_20201111.sav",sep="/"))
data <- merge(data, D15, by = 'IDC', all.x = T)
data$int3 <- data$sum_int_36m
data$ext3 <- data$sum_ext_36m
data$age3 <- data$age_GR1065/12

D16<-read_spss(paste(path,"CHILDCBCL_6_incl_Tscores_20201111.sav",sep="/"))
data <- merge(data, D16, by = 'IDC', all.x = T)
data$int6 <- data$sum_int_5
data$ext6 <- data$sum_ext_5
data$age6 <- data$agechild_GR1075/12

D17<-read_spss(paste(path,"CHILDCBCL9_incl_Tscores_20201111.sav",sep="/"))
data <- merge(data, D17, by = 'IDC', all.x = T)
data$int9 <- data$sum_int_9m
data$ext9 <- data$sum_ext_9m
data$age9 <- data$AgeChild_CBCL9m

D18<-read_spss(paste(path,"GR1093-E1_CBCL_18062020.sav",sep="/"))
data <- merge(data, D18, by = 'IDC', all.x = T)
data$int13 <- data$sum_int_14
data$ext13 <- data$sum_ext_14
data$age13 <- data$AGECHILD_GR1093

D19<-read_spss(paste(path,"GR1084_F1_24012017.sav",sep="/"))
data <- merge(data, D19, by = 'IDC', all.x = T)
data$int9s <- data$sum_int_9s
data$ext9s <- data$sum_ext_9s
data$age9s <- data$ageChildGR1084

D20<-read_spss(paste(path,"GR1095_F-Child-YSR_18062020.sav",sep="/"))
data <- merge(data, D20, by = 'IDC', all.x = T)
data$int13s <- data$sum_internalizing_13s
data$ext13s <- data$sum_externalizing_13s
data$age13s <- data$AGECHILD_GR1095

D21<-read_spss(paste(path,"CHILD-RANDOMSAMPLE_11012013.sav",sep="/"))
data <- merge(data, D21, by = 'IDC', all.x = T)
data$randomchild <- data$SELECT

D22<-read_spss(paste(path,"GR1001-F11-12_22112016.sav",sep="/"))
data <- merge(data, D22, by = 'IDM', all.x = T)
data$softdrugs <- as.factor(data$softdrugsm) # 0=no, 1=before preg, 2=during preg
data$harddrugs <- as.factor(data$harddrugsm) # 0=no, 1=before preg, 2=during preg

table(data$planned, data$folic_acid) # n=104 reported unplanned pregnancy but took folic acid prior to pregnancy
table(data$planned, data$contraceptive_use) # n=173 reported planned pregnancy but still used contraceptives
data$rm1 <- ifelse(data$folic_acid==3 & data$planned==1, 1, NA)
data2 <- data[is.na(data$rm1),] # N=9794
data2$rm2 <- ifelse(data2$contraceptive_use==1 & data2$planned==0, 1, NA)
data3 <- data2[is.na(data2$rm2),] # N=9621

library(dplyr)

df <- data3 %>%
  select("int2", "int3", "int6", "int9", "int9s", "int13", "int13s",            #outcomes
              "ext2", "ext3", "ext6", "ext9", "ext9s", "ext13", "ext13s",       #outcomes
              "UP","EPDS", "socialsupport",                                     #determinant + intervention
             "sex", "age2", "age3", "age6", "age9", "age9s","age13", "age13s",  #covariates
                "age_M", "ethniM", "educM", "income", "maritalstatus",          #covariates
              "depression_everM", "depression_everF", "anxiety_everM",          #covariates
              "anxiety_everF","ED_everM", "drinking",  "smoking",               #covariates
                "softdrugs", "harddrugs", "gest_age",                           #covariates
              "zipcode", "parity",  "BMI_M", "folic_acid", "sexpartners",       #auxiliary variables
              "ethniC", "birthweight", "apgar", "randomchild")                  #auxiliary variables

#### OPEN DATASET ####
#save(df, file="df.RData")
#load("df.RData")

####################################################################################################################
#############################################       TABLE 1       ##################################################
####################################################################################################################
library(arsenal)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,pMiss)

# Set the variables and control options
my_controls <- list(test = FALSE, total = FALSE, numeric.stats = c("meansd", "N"), cat.stats = c("countpct", "N"), digits = 2)

# Generate a table to check al variables using tableby
check <- tableby( ~ ., data= df, control = my_controls)

# Save the table to a Word document
write2word(check, "check.docx")

# Generate a general characteristics table using tableby
table_zero <- tableby( ~ UP + age_M + maritalstatus + ethniM + educM + income + parity, data= df, control = my_controls) # descriptives full cohort

# Save the table to a Word document
write2word(table_zero, "generalcharacteristics.docx")

# Generate the table using tableby
table_one <- tableby(UP ~ age_M + maritalstatus + ethniM + educM + income + parity + socialsupport + EPDS, 
                     data = df, control = my_controls)

# Save the table to a Word document
write2word(table_one, "table1.docx")

####################################################################################################################
############################################# MULTIPLE IMPUTATION ##################################################
####################################################################################################################

## Show complete cases
Data_complete <-df[complete.cases(df),]
dim(Data_complete) #N=467

# Create new variables that indicate whether the most important variables are missing or not
df$missing_epds <- ifelse(is.na(df$UP) | is.na(df$EPDS) | (is.na(df$int2) & is.na(df$int3) & is.na(df$int6) & is.na(df$int9) & is.na(df$int13) & is.na(df$int9s) & is.na(df$int13s) &
                          is.na(df$ext2) & is.na(df$ext3) & is.na(df$ext6) & is.na(df$ext9) & is.na(df$ext13) & is.na(df$ext9s) & is.na(df$ext13s)), 0, 1)
table(df$missing_epds) # n=4244
df$missing_socialsupport <- ifelse(is.na(df$UP) | is.na(df$socialsupport) | (is.na(df$int2) & is.na(df$int3) & is.na(df$int6) & is.na(df$int9) & is.na(df$int13) & is.na(df$int9s) & is.na(df$int13s) &
                                   is.na(df$ext2) & is.na(df$ext3) & is.na(df$ext6) & is.na(df$ext9) & is.na(df$ext13) & is.na(df$ext9s) & is.na(df$ext13s)), 0, 1)
table(df$missing_socialsupport) # n=3484

# Impute data
imputed.data <- mice(df, m = 30, maxit = 100, seed = 500)
summary(imputed.data)

########### SAVE FILE ################
#saveRDS(imputed.data, "O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Consequences_child\\imputed.data.rds")
########### OPEN FILE #################
#imputed.data <- readRDS("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Consequences_child\\imputed.data.rds")

###############################################################################################################
########################## Standardize outcome variables in imputed dataset ###################################
###############################################################################################################
library(mice)
long <- complete(imputed.data, action='long', include=T)
long$int2z <- as.numeric(scale(long$int2))
long$int3z <- as.numeric(scale(long$int3))
long$int6z <- as.numeric(scale(long$int6))
long$int9z <- as.numeric(scale(long$int9))
long$int13z <- as.numeric(scale(long$int13))
long$int9sz <- as.numeric(scale(long$int9s))
long$int13sz <- as.numeric(scale(long$int13s))
long$ext2z <- as.numeric(scale(long$ext2))
long$ext3z <- as.numeric(scale(long$ext3))
long$ext6z <- as.numeric(scale(long$ext6))
long$ext9z <- as.numeric(scale(long$ext9))
long$ext13z <- as.numeric(scale(long$ext13))
long$ext9sz <- as.numeric(scale(long$ext9s))
long$ext13sz <- as.numeric(scale(long$ext13s))
tertiles <- quantile(long$socialsupport, probs = c(0, 1/3, 2/3, 1), na.rm = T)
long$socialsupport_cat <- ifelse(long$socialsupport <= tertiles[2], 1, 0) # 0=sufficient social support, 1=insufficient social support
imputed.data2 <- as.mids(long)

###############################################################################################################
##########################            Determine inequality                   ##################################
###############################################################################################################

outcomes <- c("int2z","int3z","int6z","int9z","int13z","int9sz","int13sz",
              "ext2z","ext3z","ext6z","ext9z","ext13z","ext9sz","ext13sz")

output.list <- list()

for (out in outcomes) {
  if (grepl("t2", out)) {
    agecov <- "age2"
  } else if (grepl("t3", out)) {
    agecov <- "age3"
  } else if (grepl("t6", out)) {
    agecov <- "age6"
  } else if (grepl("t9z", out)) {
    agecov <- "age9"
  } else if (grepl("t9sz", out)) {
    agecov <- "age9s"
  } else if (grepl("t13z", out)) {
    agecov <- "age13"
  } else if (grepl("t13sz", out)) {
    agecov <- "age13s"
  }
  
  model_formula <- paste(out, "~ UP +", agecov, "+ sex")
  model <- with(imputed.data2, lm(formula = as.formula(model_formula)))
  
  output <- summary(pool(model), conf.int = TRUE)
  
  output.list[[length(output.list) + 1]] <- output
  names(output.list)[length(output.list)] <- paste0("out", out, "_agecov", agecov)
}

# save list #
#write.csv(output.list, file = "inequalities.csv", row.names = T) 
#regression_results <- read.csv("inequalities.csv")

# Create figures #
library(readxl)
library(ggplot2)

data_figure <- read_excel("dataset_figuur.xlsx")
#data_figure$age <- factor(data_figure$age)
ggplot(data_figure, aes(x = age)) +
  geom_line(aes(y = beta0, color = "Gepland"), size = 1.5) +
  geom_line(aes(y = beta1_int, color = "Ongepland en gewenst"), size = 1.5) +
  geom_line(aes(y = beta2_int, color = "Ongepland en in eerste instantie gemengde gevoelens"), size = 1.5) +
  geom_line(aes(y = beta3_int, color = "Ongepland en langdurige gemengde gevoelens"), size = 1.5) +
  geom_errorbar(aes(ymin = 0, ymax = 0, color = "Gepland"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow1_int, ymax = confhigh1_int, color = "Ongepland en gewenst"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow2_int, ymax = confhigh2_int, color = "Ongepland en in eerste instantie gemengde gevoelens"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow3_int, ymax = confhigh3_int, color = "Ongepland en langdurige gemengde gevoelens"), size= 0.3, width = 0.3, linetype = "dashed") +
  labs(
    x = "Leeftijd",
    y = "Internalizerende gedragsproblemen",
    color = "Group"
  ) +
  scale_color_manual(values = c(
    "Gepland" = "darkblue",
    "Ongepland en gewenst" = "#0077BE",
    "Ongepland en in eerste instantie gemengde gevoelens" = "lightgreen",
    "Ongepland en langdurige gemengde gevoelens" = "darkgreen"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "white")
  )

ggplot(data_figure, aes(x = age)) +
  geom_line(aes(y = beta0, color = "Gepland"), size = 1.5) +
  geom_line(aes(y = beta1_ext, color = "Ongepland en gewenst"), size = 1.5) +
  geom_line(aes(y = beta2_ext, color = "Ongepland en in eerste instantie gemengde gevoelens"), size = 1.5) +
  geom_line(aes(y = beta3_ext, color = "Ongepland en langdurige gemengde gevoelens"), size = 1.5) +
  geom_errorbar(aes(ymin = 0, ymax = 0, color = "Gepland"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow1_ext, ymax = confhigh1_ext, color = "Ongepland en gewenst"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow2_ext, ymax = confhigh2_ext, color = "Ongepland en in eerste instantie gemengde gevoelens"), size= 0.3, width = 0.3, linetype = "dashed") +
  geom_errorbar(aes(ymin = conflow3_ext, ymax = confhigh3_ext, color = "Ongepland en langdurige gemengde gevoelens"), size= 0.3, width = 0.3, linetype = "dashed") +
  labs(
    x = "Leeftijd",
    y = "Externalizerende gedragsproblemen",
    color = "Group"
  ) +
  scale_color_manual(values = c(
    "Gepland" = "darkblue",
    "Ongepland en gewenst" = "#0077BE",
    "Ongepland en in eerste instantie gemengde gevoelens" = "lightgreen",
    "Ongepland en langdurige gemengde gevoelens" = "darkgreen"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "white")
  )


# Create the forest plot #
data_figure <- read_excel("dataset_figuur2.xlsx")
forest_plot1 <- ggplot(data_figure, aes(y = age, x = beta_int, xmin = conflow_int, xmax = confhigh_int, color = UP, fill=UP)) +
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  labs(title = "Internalizing Behaviour",
       x = "Beta-coefficient and 95% CI",
       y = "Age") +
  scale_fill_manual(values = c("#000000","#000000","#000000","#000000"),
                    labels = c("0" = "Planned", "1" = "Unplanned & wanted", "2" = "Unplanned & initially ambivalent", "3" = "Unplanned & prolonged ambivalent")) +
  scale_color_manual(values = c("0" = "#000000", "1" = "#56B4E9", "2" = "#009E73", "3" = "#0072B2"),
                     labels = c("0" = "Planned", "1" = "Unplanned & wanted", "2" = "Unplanned & initially ambivalent", "3" = "Unplanned & prolonged ambivalent")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "white")
  )
print(forest_plot1)

forest_plot2 <- ggplot(data_figure, aes(y = age, x = beta_ext, xmin = conflow_ext, xmax = confhigh_ext, color = UP, fill=UP)) +
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_point(size=3, shape=21, colour="white", stroke = 0.5,position=position_dodge(width = 0.5)) +
  labs(title = "Externalizing Behaviour",
       x = "Beta-coefficient and 95% CI",
       y = "Age") +
  scale_fill_manual(values = c("#000000","#000000","#000000","#000000"),
                    labels = c("0" = "Planned", "1" = "Unplanned & wanted", "2" = "Unplanned & initially ambivalent", "3" = "Unplanned & prolonged ambivalent")) +
  scale_color_manual(values = c("0" = "#000000", "1" = "#56B4E9", "2" = "#009E73", "3" = "#0072B2"),
                     labels = c("0" = "Planned", "1" = "Unplanned & wanted", "2" = "Unplanned & initially ambivalent", "3" = "Unplanned & prolonged ambivalent")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "white")
  )
print(forest_plot2)


###############################################################################################################
########################## Hypothetical intervention EPDS and social support ##################################
###############################################################################################################
library(boot)
library(broom)


############################################ LOOP ####################################################

outcomes    <- c("int2z","int3z","int6z","int9z","int13z", 
                 "ext2z","ext3z","ext6z","ext9z","ext13z") 

mediators   <- c("EPDS","socialsupport_cat")

output.list <- list()

for(out in outcomes){
  for(med in mediators){

    for(imp in 1:30){

      ds <- complete(imputed.data2, imp)
      
        if (grepl("t2z", out)) {
          agecov <- "age2"
        } else if (grepl("t3z", out)) {
          agecov <- "age3"
        } else if (grepl("t6z", out)) {
          agecov <- "age6"
        } else if (grepl("t9z", out)) {
          agecov <- "age9"
        } else if (grepl("t13z", out)) {
          agecov <- "age13"
        } 
      
      ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = out, mediator = med, 
               ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",  
                                             "depression_everM", "depression_everF", "anxiety_everM",          
                                             "anxiety_everF","ED_everM", "drinking", "smoking",               
                                             "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
               set_mediator_to = list(paste0(med,"==0")),
               params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))
      

      boot_out <- boot(data = ds, statistic = ineq_fun, outcome = out, mediator = med,
                       ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",
                                                     "depression_everM", "depression_everF", "anxiety_everM",
                                                     "anxiety_everF","ED_everM", "drinking",  "smoking",
                                                     "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
               set_mediator_to = list(paste0(med,"==0")), R=1000,
               params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))

      output <- tidy(boot_out, conf.int=T)

      output.list[[length(output.list) + 1]]   <- output
      names(output.list)[length(output.list)]  <- paste0("out",out,"_agecov",agecov,"_med",med,"_imp",imp)
      
    }
  }
}

#check length of list
# length(output.list)     #check length of list
# output.list[[1]]        #check first list based on number
# output.list[["outint2z_agecovage2_medEPDS_imp1"]]   #check first list based on name

# save list #
write.csv(output.list, file = "output_hypotheticalintervention.csv", row.names = T) 

### select separate analyses from list ####

# internalizing problems and EPDS #
output.list.int_epds2 <- output.list[which(grepl("outint2z_agecovage2_medEPDS", names(output.list)))]
output.list.int_epds3 <- output.list[which(grepl("outint3z_agecovage3_medEPDS", names(output.list)))]
output.list.int_epds6 <- output.list[which(grepl("outint6z_agecovage6_medEPDS", names(output.list)))]
output.list.int_epds9 <- output.list[which(grepl("outint9z_agecovage9_medEPDS", names(output.list)))]
output.list.int_epds13 <- output.list[which(grepl("outint13z_agecovage13_medEPDS", names(output.list)))]

# externalizing problems and EPDS #
output.list.ext_epds2 <- output.list[which(grepl("outext2z_agecovage2_medEPDS", names(output.list)))]
output.list.ext_epds3 <- output.list[which(grepl("outext3z_agecovage3_medEPDS", names(output.list)))]
output.list.ext_epds6 <- output.list[which(grepl("outext6z_agecovage6_medEPDS", names(output.list)))]
output.list.ext_epds9 <- output.list[which(grepl("outext9z_agecovage9_medEPDS", names(output.list)))]
output.list.ext_epds13 <- output.list[which(grepl("outext13z_agecovage13_medEPDS", names(output.list)))]

# internalizing problems and social support #
output.list.int_socialsupport2 <- output.list[which(grepl("outint2z_agecovage2_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport3 <- output.list[which(grepl("outint3z_agecovage3_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport6 <- output.list[which(grepl("outint6z_agecovage6_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport9 <- output.list[which(grepl("outint9z_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport13 <- output.list[which(grepl("outint13z_agecovage13_medsocialsupport_cat", names(output.list)))]

# externalizing problems and social support #
output.list.ext_socialsupport2 <- output.list[which(grepl("outext2z_agecovage2_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport3 <- output.list[which(grepl("outext3z_agecovage3_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport6 <- output.list[which(grepl("outext6z_agecovage6_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport9 <- output.list[which(grepl("outext9z_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport13 <- output.list[which(grepl("outext13z_agecovage13_medsocialsupport_cat", names(output.list)))]

## pool results ##

# function to extract the values from the output list #
extract_output <- function(output_list, term, measure) {
  sapply(output_list, function(x) {
    value <- as.data.frame(x)[x$term == term, measure]
    if (length(value) > 0) value else NA
  })
}

# Function to calculate the mean across lists for a specific term and measure
calculate_mean <- function(output_list, term, measure) {
  values <- extract_output(output_list, term, measure)
  mean_value <- mean(unlist(values), na.rm = TRUE)
  mean_value
}

# Define the list names
list_names <- c("int_epds2", "int_epds3", "int_epds6", "int_epds9", "int_epds13",  
                "ext_epds2", "ext_epds3", "ext_epds6", "ext_epds9", "ext_epds13",
                "int_socialsupport2", "int_socialsupport3", "int_socialsupport6", "int_socialsupport9", "int_socialsupport13",  
                "ext_socialsupport2", "ext_socialsupport3", "ext_socialsupport6", "ext_socialsupport9",  "ext_socialsupport13")

# Loop over the lists
for (list_name in list_names) {
  output_list <- get(paste0("output.list.", list_name))
  
  # Create a dataframe to store the mean values
  pooled_result <- data.frame(
    term = c("pre_0 vs 1", "pre_0 vs 2", "pre_0 vs 3",
             "post_0 vs 1", "post_0 vs 2", "post_0 vs 3",
             "diff_post_0 vs 1", "diff_post_0 vs 2", "diff_post_0 vs 3",
             "ratio_post_0 vs 1", "ratio_post_0 vs 2", "ratio_post_0 vs 3"),
    statistic = numeric(length = 12),
    conf.low = numeric(length = 12),
    conf.high = numeric(length = 12)
  )
  
  # Loop over the rows and calculate the means for the specified columns
  for (i in 1:nrow(pooled_result)) {
    term <- pooled_result$term[i]
    pooled_result$statistic[i] <- calculate_mean(output_list, term, "statistic")
    pooled_result$conf.low[i] <- calculate_mean(output_list, term, "conf.low")
    pooled_result$conf.high[i] <- calculate_mean(output_list, term, "conf.high")
  }
  
  # Save the results to a CSV file
  write.csv(pooled_result, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)
}


############################################################################################################
#####################################          sensitivity analyses     ####################################
############################################################################################################
# Perform with self-reported internalizing and externalizing behaviour at ages 9 and 13
outcomes    <- c("int9sz","int13sz", 
                 "ext9sz","ext13sz") 

mediators   <- c("EPDS","socialsupport_cat")

output.list <- list()

for(out in outcomes){
  for(med in mediators){
    
    for(imp in 1:30){
      
      ds <- complete(imputed.data2, imp)
      
      if (grepl("t9sz", out)) {
        agecov <- "age9s"
      } else if (grepl("t13sz", out)) {
        agecov <- "age13s"
      }
      
      ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = out, mediator = med, 
               ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",  
                                             "depression_everM", "depression_everF", "anxiety_everM",          
                                             "anxiety_everF","ED_everM", "drinking", "smoking",               
                                             "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
               set_mediator_to = list(paste0(med,"==0")),
               params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))
      
      boot_out <- boot(data = ds, statistic = ineq_fun, outcome = out, mediator = med,
                       ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",
                                                     "depression_everM", "depression_everF", "anxiety_everM",
                                                     "anxiety_everF","ED_everM", "drinking",  "smoking",
                                                     "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
                       set_mediator_to = list(paste0(med,"==0")), R=1000,
                       params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))
      
      output <- tidy(boot_out, conf.int=T)
      
      output.list[[length(output.list) + 1]]   <- output
      names(output.list)[length(output.list)]  <- paste0("out",out,"_agecov",agecov,"_med",med,"_imp",imp)
      
    }
  }
}

#check length of list
# length(output.list)     #check length of list
# output.list[[1]]        #check first list based on number
# output.list[["outint9sz_agecovage9s_medEPDS_imp1"]]   #check first list based on name

# save list #
write.csv(output.list, file = "output_hypotheticalintervention_sens1.csv", row.names = T) 

### select separate analyses from list ####

# internalizing problems and EPDS #
output.list.int_epds9s <- output.list[which(grepl("outint9sz_agecovage9_medEPDS", names(output.list)))]
output.list.int_epds13s <- output.list[which(grepl("outint13sz_agecovage13_medEPDS", names(output.list)))]

# externalizing problems and EPDS #
output.list.ext_epds9s <- output.list[which(grepl("outext9sz_agecovage9_medEPDS", names(output.list)))]
output.list.ext_epds13s <- output.list[which(grepl("outext13sz_agecovage13_medEPDS", names(output.list)))]

# internalizing problems and social support #
output.list.int_socialsupport9s <- output.list[which(grepl("outint9sz_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport13s <- output.list[which(grepl("outint13sz_agecovage13_medsocialsupport_cat", names(output.list)))]

# externalizing problems and social support #
output.list.ext_socialsupport9s <- output.list[which(grepl("outext9sz_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport13s <- output.list[which(grepl("outext13sz_agecovage13_medsocialsupport_cat", names(output.list)))]

## pool results ##

# function to extract the values from the output list #
extract_output <- function(output_list, term, measure) {
  sapply(output_list, function(x) {
    value <- as.data.frame(x)[x$term == term, measure]
    if (length(value) > 0) value else NA
  })
}

# Function to calculate the mean across lists for a specific term and measure
calculate_mean <- function(output_list, term, measure) {
  values <- extract_output(output_list, term, measure)
  mean_value <- mean(unlist(values), na.rm = TRUE)
  mean_value
}

# Define the list names
list_names <- c( "int_epds9s", "int_epds13s", "ext_epds9s", "ext_epds13s",
                "int_socialsupport9s", "int_socialsupport13s", "ext_socialsupport9s", "ext_socialsupport13s")

# Loop over the lists
for (list_name in list_names) {
  output_list <- get(paste0("output.list.", list_name))
  
  # Create a dataframe to store the mean values
  pooled_result <- data.frame(
    term = c("pre_0 vs 1", "pre_0 vs 2", "pre_0 vs 3",
             "post_0 vs 1", "post_0 vs 2", "post_0 vs 3",
             "diff_post_0 vs 1", "diff_post_0 vs 2", "diff_post_0 vs 3",
             "ratio_post_0 vs 1", "ratio_post_0 vs 2", "ratio_post_0 vs 3"),
    statistic = numeric(length = 12),
    conf.low = numeric(length = 12),
    conf.high = numeric(length = 12)
  )
  
  # Loop over the rows and calculate the means for the specified columns
  for (i in 1:nrow(pooled_result)) {
    term <- pooled_result$term[i]
    pooled_result$statistic[i] <- calculate_mean(output_list, term, "statistic")
    pooled_result$conf.low[i] <- calculate_mean(output_list, term, "conf.low")
    pooled_result$conf.high[i] <- calculate_mean(output_list, term, "conf.high")
  }
  
  # Save the results to a CSV file
  write.csv(pooled_result, file = paste0("pooled.result.sens1", list_name, ".csv"), row.names = FALSE)
}

# Exclude random child in case of multiple pregnancies
table(ds$randomchild) # select random child = 1

# Exclude participants with missing data on EPDS, UP and all outcomes
table(ds$missing_epds) # select missing_epds = 1

# Exclude participants with missing data on social support, UP and all outcomes
table(ds$missing_epds) # select missing_epds = 1

outcomes    <- c("int2z","int3z","int6z","int9z","int13z","int9sz","int13sz", 
                 "ext2z","ext3z","ext6z","ext9z","ext13z","ext9sz","ext13sz") 

mediators   <- c("EPDS","socialsupport_cat")

output.list <- list()

for(out in outcomes){
  for(med in mediators){
    
    for(imp in 1:2){
      
      ds <- complete(imputed.data2, imp)
      
      if (grepl("t2", out)) {
        agecov <- "age2"
      } else if (grepl("t3", out)) {
        agecov <- "age3"
      } else if (grepl("t6", out)) {
        agecov <- "age6"
      } else if (grepl("t9z", out)) {
        agecov <- "age9"
      } else if (grepl("t9sz", out)) {
        agecov <- "age9s"
      } else if (grepl("t13z", out)) {
        agecov <- "age13"
      } else if (grepl("t13sz", out)) {
        agecov <- "age13s"
      }
      
      ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = out, mediator = med, 
               ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",  
                                             "depression_everM", "depression_everF", "anxiety_everM",          
                                             "anxiety_everF","ED_everM", "drinking", "smoking",               
                                             "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
               set_mediator_to = list(paste0(med,"==0")),
               params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))
      
      
      boot_out <- boot(data = ds, statistic = ineq_fun, outcome = out, mediator = med,
                       ineq_group = "UP", covars = c("sex", agecov, "age_M", "ethniM", "educM", "income", "maritalstatus",
                                                     "depression_everM", "depression_everF", "anxiety_everM",
                                                     "anxiety_everF","ED_everM", "drinking",  "smoking",
                                                     "softdrugs", "harddrugs", "gest_age"), interaction = TRUE,
                       set_mediator_to = list(paste0(med,"==0")), R=2,
                       params=list(adjustment=c(agecov,"sex"),ineq_group="UP",outcome=out))
      
      output <- tidy(boot_out, conf.int=T)
      
      output.list[[length(output.list) + 1]]   <- output
      names(output.list)[length(output.list)]  <- paste0("out",out,"_agecov",agecov,"_med",med,"_imp",imp)
      
    }
  }
}

#check length of list
# length(output.list)     #check length of list
# output.list[[1]]        #check first list based on number
# output.list[["outint2z_agecovage2_medEPDS_imp1"]]   #check first list based on name
output.list[["outint9sz_agecovage9_medEPDS_imp1"]] 

# save list #
#write.csv(output.list, file = "output_hypotheticalintervention.csv", row.names = T) 

### select separate analyses from list ####

# internalizing problems and EPDS #
output.list.int_epds2 <- output.list[which(grepl("outint2z_agecovage2_medEPDS", names(output.list)))]
output.list.int_epds3 <- output.list[which(grepl("outint3z_agecovage3_medEPDS", names(output.list)))]
output.list.int_epds6 <- output.list[which(grepl("outint6z_agecovage6_medEPDS", names(output.list)))]
output.list.int_epds9 <- output.list[which(grepl("outint9z_agecovage9_medEPDS", names(output.list)))]
output.list.int_epds9s <- output.list[which(grepl("outint9sz_agecovage9_medEPDS", names(output.list)))]
output.list.int_epds13 <- output.list[which(grepl("outint13z_agecovage13_medEPDS", names(output.list)))]
output.list.int_epds13s <- output.list[which(grepl("outint13sz_agecovage13_medEPDS", names(output.list)))]

# externalizing problems and EPDS #
output.list.ext_epds2 <- output.list[which(grepl("outext2z_agecovage2_medEPDS", names(output.list)))]
output.list.ext_epds3 <- output.list[which(grepl("outext3z_agecovage3_medEPDS", names(output.list)))]
output.list.ext_epds6 <- output.list[which(grepl("outext6z_agecovage6_medEPDS", names(output.list)))]
output.list.ext_epds9 <- output.list[which(grepl("outext9z_agecovage9_medEPDS", names(output.list)))]
output.list.ext_epds9s <- output.list[which(grepl("outext9sz_agecovage9_medEPDS", names(output.list)))]
output.list.ext_epds13 <- output.list[which(grepl("outext13z_agecovage13_medEPDS", names(output.list)))]
output.list.ext_epds13s <- output.list[which(grepl("outext13sz_agecovage13_medEPDS", names(output.list)))]

# internalizing problems and social support #
output.list.int_socialsupport2 <- output.list[which(grepl("outint2z_agecovage2_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport3 <- output.list[which(grepl("outint3z_agecovage3_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport6 <- output.list[which(grepl("outint6z_agecovage6_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport9 <- output.list[which(grepl("outint9z_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport9s <- output.list[which(grepl("outint9sz_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport13 <- output.list[which(grepl("outint13z_agecovage13_medsocialsupport_cat", names(output.list)))]
output.list.int_socialsupport13s <- output.list[which(grepl("outint13sz_agecovage13_medsocialsupport_cat", names(output.list)))]

# externalizing problems and social support #
output.list.ext_socialsupport2 <- output.list[which(grepl("outext2z_agecovage2_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport3 <- output.list[which(grepl("outext3z_agecovage3_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport6 <- output.list[which(grepl("outext6z_agecovage6_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport9 <- output.list[which(grepl("outext9z_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport9s <- output.list[which(grepl("outext9sz_agecovage9_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport13 <- output.list[which(grepl("outext13z_agecovage13_medsocialsupport_cat", names(output.list)))]
output.list.ext_socialsupport13s <- output.list[which(grepl("outext13sz_agecovage13_medsocialsupport_cat", names(output.list)))]

## pool results ##

# function to extract the values from the output list #
extract_output <- function(output_list, term, measure) {
  sapply(output_list, function(x) {
    value <- as.data.frame(x)[x$term == term, measure]
    if (length(value) > 0) value else NA
  })
}

# Function to calculate the mean across lists for a specific term and measure
calculate_mean <- function(output_list, term, measure) {
  values <- extract_output(output_list, term, measure)
  mean_value <- mean(unlist(values), na.rm = TRUE)
  mean_value
}

# Define the list names
list_names <- c("int_epds2", "int_epds3", "int_epds6", "int_epds9", "int_epds9s", "int_epds13", "int_epds13s", 
                "ext_epds2", "ext_epds3", "ext_epds6", "ext_epds9", "ext_epds9s", "ext_epds13", "ext_epds13s",
                "int_socialsupport2", "int_socialsupport3", "int_socialsupport6", "int_socialsupport9", "int_socialsupport9s", "int_socialsupport13", "int_socialsupport13s", 
                "ext_socialsupport2", "ext_socialsupport3", "ext_socialsupport6", "ext_socialsupport9", "ext_socialsupport9s", "ext_socialsupport13", "ext_socialsupport13s")

# Loop over the lists
for (list_name in list_names) {
  output_list <- get(paste0("output.list.", list_name))
  
  # Create a dataframe to store the mean values
  pooled_result <- data.frame(
    term = c("pre_0 vs 1", "pre_0 vs 2", "pre_0 vs 3",
             "post_0 vs 1", "post_0 vs 2", "post_0 vs 3",
             "diff_post_0 vs 1", "diff_post_0 vs 2", "diff_post_0 vs 3",
             "ratio_post_0 vs 1", "ratio_post_0 vs 2", "ratio_post_0 vs 3"),
    statistic = numeric(length = 12),
    conf.low = numeric(length = 12),
    conf.high = numeric(length = 12)
  )
  
  # Loop over the rows and calculate the means for the specified columns
  for (i in 1:nrow(pooled_result)) {
    term <- pooled_result$term[i]
    pooled_result$statistic[i] <- calculate_mean(output_list, term, "statistic")
    pooled_result$conf.low[i] <- calculate_mean(output_list, term, "conf.low")
    pooled_result$conf.high[i] <- calculate_mean(output_list, term, "conf.high")
  }
  
  # Save the results to a CSV file
  write.csv(pooled_result, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)
}

