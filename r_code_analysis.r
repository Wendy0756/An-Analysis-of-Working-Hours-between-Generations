library(foreign)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggplot2)
library(patchwork)
library(GGally)
library(RColorBrewer)
library(caret)
library(stargazer)

setwd("C:/Users/xueyi/Downloads/data/ds203_GSS")
#setwd("C:/Users/miles/Downloads/model_1/batch_0/result")

  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.table(dat, col.names = metadata_var[["ColName"]])
  }


GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii
glimpse(GSS)

# Relabel char features: sex, wrkslf, marital, reg16, rincome, yearsjob
gss_filtered = GSS %>%
  mutate(SEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female",
    TRUE ~ NA
  ),
  WRKSLF = case_when(
    WRKSLF == 1 ~ "Self-employed",
    WRKSLF == 2 ~ "Someone-else",
    TRUE ~ NA
  ),
  MARITAL = case_when(
    MARITAL == 1 ~ "Married",
    MARITAL == 2 ~ "Widowed",
    MARITAL == 3 ~ "Divorced",
    MARITAL == 4 ~ "Separated",
    MARITAL == 5 ~ "Never married",
    TRUE ~ NA
  ),
  REG16 = case_when(
    REG16 == 9 ~ "PACIFIC",
    REG16 == 8 ~ "MOUNTAIN",
    REG16 == 7 ~ "W. SOU. CENTRAL",
    REG16 == 6 ~ "E. SOU. CENTRAL",
    REG16 == 5 ~ "SOUTH ATLANTIC",
    REG16 == 4 ~ "W. NOR. CENTRAL",
    REG16 == 3 ~ "E. NOR. CENTRAL",
    REG16 == 2 ~ "MIDDLE ATLANTIC",
    REG16 == 1 ~ "NEW ENGLAND",
    REG16 == 0 ~ "FOREIGN",
    TRUE ~ NA
  ),
  RINCOME = case_when(
    RINCOME == 12 ~ "$25000 OR MORE",
    RINCOME == 11 ~ "$20000 - 24999",
    RINCOME == 10 ~ "$15000 - 19999",
    RINCOME == 9 ~ "$10000 - 14999",
    RINCOME == 8 ~ "$8000 TO 9999",
    RINCOME == 7 ~ "$7000 TO 7999",
    RINCOME == 6 ~ "$6000 TO 6999",
    RINCOME == 5 ~ "$5000 TO 5999",
    RINCOME == 4 ~ "$4000 TO 4999",
    RINCOME == 3 ~ "$3000 TO 3999",
    RINCOME == 2 ~ "$1000 TO 2999",
    RINCOME == 1 ~ "LT $1000",
    TRUE ~ NA
  )
)

# ======== Exploring: Not used ============================================================
gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
)%>%count()
#   n
#1 1393

gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female")
)%>%count()
#     n
#1 1392


gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else")
)%>%count()
#     n
#1 1389

gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
)%>%count()
#     n
#1 1389

gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
)%>%count()
#     n
#1 1389


gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)%>%count()
#n
#1 1181


gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$SPHRS2 >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)%>%count()
#n
#123

gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$CHILDS >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)%>%count()
#n
#1 1177



gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$CHILDS >= 0,
  gss_filtered$YEARSJOB >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)%>%count()
#
#1 241

# ======== Exploring: Not used ============================================================
  
## creating train test set 
### without the following vars: SPHRS2 & YEARSJOB

gss_filtered %>% filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$CHILDS >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)%>%count() # 1177



# create model data 
num_cols = c("AGE", "HRS2", "CHILDS")
cat_cols = c("SEX", 	"WRKSLF",	"MARITAL", "REG16",	"RINCOME", "YEAR")


paste(num_cols, cat_cols)

mod_data = gss_filtered %>% 
  select(all_of(c(num_cols, cat_cols)))%>%
  filter(
  gss_filtered$AGE >= 0,
  gss_filtered$HRS2 >= 0,
  gss_filtered$CHILDS >= 0,
  gss_filtered$SEX %in% c("Male","Female"),
  gss_filtered$WRKSLF %in% c("Self-employed", "Someone-else"),
  is.na(gss_filtered$MARITAL) == F,
  is.na(gss_filtered$REG16) == F,
  is.na(gss_filtered$RINCOME) == F,
)

mod_data %>% count() #1177

##  create train test split 
#### 75% of the sample size
smp_size <- floor(0.75 * nrow(mod_data))

#### set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mod_data)), size = smp_size)

#### creating train test data
train <- mod_data[train_ind, ]
test <- mod_data[-train_ind, ]

train%>%count() #882
test %>% count() #295


## Exploratory Plots 





####### Work Hours vs Numerical Var Plots #########
hrs_hist = train %>%
ggplot(aes(x= HRS2))+
  geom_histogram(fill = "light blue" )+
  scale_x_continuous(breaks=seq(0,90,10))+
  #scale_y_continuous(breaks=seq(0,80, 10))+
  ggtitle("Work Hours Histogram")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )

hrs_hist

hrs_vs_age = train %>%
  ggplot(aes(y= HRS2, x = AGE))+
  geom_jitter(color = "light blue" )+
  geom_smooth(method = 'lm', se= F) +
  scale_x_continuous(breaks=seq(0,90,10))+
  scale_y_continuous(breaks=seq(0,90, 10))+
  ggtitle("Work Hours vs Age Scatter")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )
hrs_vs_age

hrs_vs_childs = train %>%
  ggplot(aes(y= HRS2, x = CHILDS))+
  geom_jitter(color = "light blue" )+
  geom_smooth(method = 'lm', se= F) +
  scale_x_continuous(breaks=seq(0,10, 1))+
  scale_y_continuous(breaks=seq(0,90, 10))+
  ggtitle("Work Hours vs Children Scatter")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )
hrs_vs_childs

hrs_vs_years = train %>%
  ggplot(aes(y= HRS2, x = YEAR))+
  geom_jitter(color = "light blue" )+
  geom_smooth(method = 'lm', se= F) +
  scale_x_continuous(breaks=seq(0,10, 1))+
  scale_y_continuous(breaks=seq(0,90, 10))+
  ggtitle("Work Hours vs Year")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )
hrs_vs_years

##### Work Hrs vs Categorical Var Plots #####
sex_box = train %>%
  ggplot(aes(x = HRS2 ))+
  geom_boxplot(aes(fill = SEX))+
  scale_x_continuous(breaks=seq(0,90,10))+
  coord_flip()+
  ggtitle("Work Hours vs Sex Boxplot")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )+scale_fill_brewer(palette = "Blues")
sex_box  


wrkslf_box <- train %>%
  ggplot(aes(x = HRS2 ))+
  geom_boxplot(aes(fill = WRKSLF))+
  scale_x_continuous(breaks=seq(0,90,10))+
  coord_flip()+
  ggtitle("Work Hours vs Employer Boxplot")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )+scale_fill_brewer(palette = "Blues")
wrkslf_box

marital_box <- train %>%
  ggplot(aes(x = HRS2 ))+
  geom_boxplot(aes(fill = MARITAL))+
  scale_x_continuous(breaks=seq(0,90,10))+
  coord_flip()+
  ggtitle("Work Hours vs Marital Status Boxplot")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )+scale_fill_brewer(palette = "Blues")
marital_box


reg16_box <- train %>%
  ggplot(aes(x = HRS2 ))+
  geom_boxplot(aes(fill = REG16))+
  scale_x_continuous(breaks=seq(0,90,10))+
  coord_flip()+
  ggtitle("Work Hours vs Region Boxplot")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )+scale_fill_brewer(palette = "Blues")
reg16_box

income_box <- train %>%
  ggplot(aes(x = HRS2 ))+
  geom_boxplot(aes(fill = RINCOME))+
  scale_x_continuous(breaks=seq(0,90,10))+
  coord_flip()+
  ggtitle("Work Hours vs Income Boxplot")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )+scale_fill_brewer(palette = "Blues")
income_box



# These are the plots of the training data
# need to export with width of 1400 w 1000 h
(hrs_hist | hrs_vs_age | hrs_vs_childs ) /
(sex_box |wrkslf_box | marital_box) /
(reg16_box | income_box) 



# These are raw Plots of the Categorical Variables mainly to see if anyhting was mis-coded
sex_bar <- train %>%
  ggplot(aes(x= SEX ))+
  geom_bar(fill = "blue", alpha = 0.5)+
  #coord_flip()+
  ggtitle("Age")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )
sex_bar

wrkslf_bar <- train %>%
  ggplot(aes(x= WRKSLF ))+
  geom_bar(fill = "blue", alpha = 0.5)+
  #coord_flip()+
  ggtitle("Employer")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )


marital_bar <- train %>%
  ggplot(aes(x= MARITAL ))+
  geom_bar(fill = "blue", alpha = 0.5)+
  #coord_flip()+
  ggtitle("Marital Status")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )


reg16_bar <- train %>%
  ggplot(aes(x= REG16 ))+
  geom_bar(fill = "blue", alpha = 0.5)+
  #coord_flip()+
  ggtitle("Region")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )


income_bar <- train %>%
  ggplot(aes(x= RINCOME ))+
  geom_bar(fill = "blue", alpha = 0.5)+
  #coord_flip()+
  ggtitle("Income")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
  )

####### Modelling #####

#some what worriesome our fitted values are in tight range relative to data (not good)
# hrs2 is weakly significant (significant at alpha .10) 
# residuals missing on both extremes (qqplot)
mod = lm(data = train, HRS2 ~ AGE)
plot(mod) # use this to look at diagnostics while you model
coeftest(mod, vcov = vcovHC(mod))
summary(mod)
#F-stat 2.738 (low)

#adding sex
#residual: got a little better slightly larger range of predicted values
#significance: sex is significant age became more significant
mod1 = lm(data = train, HRS2 ~ AGE + SEX)
plot(mod1)
coeftest(mod1, vcov = vcovHC(mod1))
summary(mod1) 
#Residual standard error: 12.53
#F-stat 36.77 (much better)


#adding interaction AGE:SEX
#residual: range of residuals got slightly better
#significance: AGE:SEX is significant but coefficient is small (keeping to see how it does with other vars)
mod2 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS)
plot(mod2)
coeftest(mod2, vcov = vcovHC(mod2))
summary(mod2) 
#F-stat 26.22
#Residual standard error: 12.51


#adding wrkself (dropping in next models)
#residuals: still missing on extremes looking at qqplot
#significance: wrkslf is not significant and is actually lowering adjusted R 
mod3 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS + WRKSLF)
plot(mod3)
coeftest(mod3, vcov = vcovHC(mod3))
summary(mod3) 
# F-stat 19.17 (not a good drop)
# Residual standard error: 12.53 (no change, not good)



#adding marital (dropping in future models)
#residual:  residuals look decent but still missing on extremes in qqplot
#significance: most levels of marital not significant
mod4 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS + MARITAL)
plot(mod4)
coeftest(mod4, vcov = vcovHC(mod4))
summary(mod4) 
# F-stat 12.08 (not a good drop from model:3)
# Residual standard error: 12.5


### best model without interactions ###
#adding income
#residual: best qqplot of residuals (decent improvement in adjusted Rsqr)
#significance: income definitely significant   
mod5 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS + RINCOME)
plot(mod5)
coeftest(mod5, vcov = vcovHC(mod5))
summary(mod5)
# F-stat 15.31 (pretty big drop but most levels are significant)
# Residual standard error: 11.76 (decent decrease from mod3)



# adding REG16 (dropping going forward)
# residuals: 
# signficance: ((also not significant -probably should drop this var)
mod6 = lm(data = train, HRS2 ~AGE + SEX  + CHILDS + RINCOME + REG16)
plot(mod6)
coeftest(mod6, vcov = vcovHC(mod6))
summary(mod6)
#F-stat 9.8 (bad drop)
#Residual standard error: 11.75

#### testing interaction using vars in mod6 ####


#adding AGE:SEX
#residual: same as mod 6
#significance: AGE:SEX not signficant  
mod7 = lm(data = train, HRS2 ~ AGE + SEX + AGE:SEX + CHILDS + RINCOME)
plot(mod7)
coeftest(mod7, vcov = vcovHC(mod7))
summary(mod7)
# F-stat 14.48 (slight decrease from mod6, not good)
# Residual standard error: 11.75 (slight decrease from mod6)


#adding AGE:CHILDS
#residual: same as mod 6
#significance: AGE:CHILDS not signficant  
mod8 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS + AGE:CHILDS + RINCOME)
plot(mod8)
coeftest(mod8, vcov = vcovHC(mod8))
summary(mod8)
# F-stat 14.42 (slight decrease from mod6, not good)
# Residual standard error: 11.76 (slight decrease from mod6)



#adding AGE:RINCOME
#residual: same as mod 6
#significance: AGE:RINCOME not signficant  
mod9 = lm(data = train, HRS2 ~ AGE + SEX + CHILDS + RINCOME + AGE:RINCOME )
plot(mod9)
coeftest(mod9, vcov = vcovHC(mod9))
summary(mod9)
# F-stat 9.433 (large decrease from mod6, not good)
# Residual standard error: 11.71 (slight decrease from mod6)


### Best overall model####
#adding polynomial I(AGE^2)
#residual: same as mod 6
#significance: AGE^2 significant  
mod10 = lm(data = train, HRS2 ~ AGE + I(AGE^2) + SEX + CHILDS + RINCOME)
plot(mod10)
coeftest(mod10, vcov = vcovHC(mod10))
summary(mod10)
# F-stat 14.99 (slight decrease from mod6, OK)
# Residual standard error: 11.71 (slight decrease from mod6, good)

par(mfrow = c(2, 2))
### NEW BEST MODEL
mod11 = lm(HRS2 ~ AGE + SEX + CHILDS + RINCOME,
           data = train %>% mutate(RES = mod10$residuals), weights = 1/exp(abs(RES)))
plot(mod11)

coeftest(mod11, vcov = vcovHC(mod11))

#======================================================================================
mod_list1 <- list(mod,mod1,mod3)
mod_list2 <- list(mod6,mod10, mod11)

stargazer(
  mod_list2,
  type = "text",
  single.row= TRUE,
  robust = TRUE,
  font.size = "footnotesize",
  column.sep.width = "1pt",
  digits=2,
  no.space = TRUE
)

bptest(mod11)
#======================================================================================

### performing the k-fold validation ###

# set seed to generate a reproducible random sample
set.seed(123)

# the number of K is set to be 5
# need caret package
train.kfold <- trainControl(method = "cv", number = 5)

### Training
model.kfold <- train(HRS2 ~ AGE + I(AGE^2) + SEX + CHILDS + RINCOME,
                     data = train,
                     method = "lm",
                     trControl = train.kfold)

### Present results
print(model.kfold)


