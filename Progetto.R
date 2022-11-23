library(tidyverse) # metapackage of all tidyverse packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)
library(plyr)
library(corrplot)
library("PerformanceAnalytics")
library(data.table)
library(rstatix)
library(AICcmodavg)
library(glmnet)
library(caret)
library(Metrics)
library(GGally)
library(MKinfer)

#read dataset
df <- read_csv('C:/Users/hp/Desktop/statistica/world-happiness-report-2021.csv', show_col_types = FALSE) 
#head(df) #see dataset
#glimpse(df) #see all columns

#Boxplot, mean and standard deviation of Happiness
boxplot(df$'Ladder score',
        main = "Mean Happiness in the World",
        xlab = "Count",
        ylab = 'Ladder score',
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE) 

#ten happiest countries
df %>% arrange(desc(`Ladder score`)) %>% head(10) %>% 
  ggplot(aes(x = `Ladder score`, y = reorder(`Country name`, `Ladder score`),  fill = `Country name`)) +
  geom_point(aes(color = `Regional indicator`)) + geom_bar(stat = "identity") +
  labs(title = "Top Ten Happiest Countries") + ylab("Countries") + xlab("Happiness Score") +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = `Ladder score`), position=position_stack(vjust=0.9),color="black",size=3)
df %>% arrange(`Ladder score`) %>% head(10) %>% 
  ggplot(aes(x = `Ladder score`, y = reorder(`Country name`, `Ladder score`),  fill = `Country name`)) + 
  geom_point(aes(color = `Regional indicator`)) + geom_bar(stat = "identity") + labs(title = "Bottom Ten Happiest Countries") +
  ylab("Coutries") + xlab("Happiness Score") + scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = `Ladder score`), position=position_stack(vjust=0.9),color="black",size=3)
#happiness in different regions 
df %>% mutate(region = fct_reorder(`Regional indicator`, `Ladder score`, median)) %>% 
  ggplot(aes(`Ladder score`)) + geom_boxplot(aes(fill=`Regional indicator`)) + coord_flip() +
  xlab("Happiness") + ylab("Region")

###histograms of variables###
p1<-  ggplot( data = df, aes(x=`Ladder score`) ) +
  geom_histogram(color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Ladder score`)),
             linetype="dashed")+ theme_bw()+
  labs(x = "Ladder score", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Ladder score") + 
  theme_minimal()

p2<-  ggplot( data = df, aes(x=`Social support`) ) +
  geom_histogram( color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Social support`)),
             linetype="dashed") + theme_bw() +
  labs(x = "Social support", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Social support") + 
  theme_minimal()

p3<-  ggplot( data = df, aes(x=`Perceptions of corruption`) ) +
  geom_histogram(color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Perceptions of corruption`)),
             linetype="dashed")+ theme_bw()+
  labs(x = "Perceptions of corruption", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Perceptions of corruption") + 
  theme_minimal()

p4<-  ggplot( data = df, aes(x=`Dystopia + residual`) ) +
  geom_histogram(color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Dystopia + residual`)),
             linetype="dashed")+ theme_bw()+
  labs(x = "Ladder score in Dystopia", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Ladder score in Dystopia") + 
  theme_minimal()

p5<-  ggplot( data = df, aes(x= `Logged GDP per capita`) ) +
  geom_histogram( color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean( `Logged GDP per capita`)),
             linetype="dashed") + theme_bw() +
  labs(x = "Logged GDP per capita", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Logged GDP per capita") + 
  theme_minimal()

p7<-  ggplot( data = df, aes(x=`Healthy life expectancy`) ) +
  geom_histogram( color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Healthy life expectancy`)),
             linetype="dashed") + theme_bw() +
  labs(x = "Healthy life expectancy", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Healthy life expectancy") + 
  theme_minimal()  

p8<-  ggplot( data = df, aes(x=`Freedom to make life choices`) ) +
  geom_histogram( color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean(`Freedom to make life choices`)),
             linetype="dashed") + theme_bw() +
  labs(x = "Freedom to make life choices", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Freedom to make life choices") + 
  theme_minimal()

p9<-  ggplot( data = df, aes(x= Generosity  ) ) +
  geom_histogram( color="black", fill="skyblue") +
  geom_vline(data=df, aes(xintercept=mean( Generosity  )),
             linetype="dashed") + theme_bw() +
  labs(x = "Generosity", y = "Count") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Generosity") + 
  theme_minimal()    

ggarrange(p1, p2, p3, p4, p5, p7, p8, p9, labels = "AUTO")  

#select columns of interest
df1 <- df[,-(1:2),drop=FALSE]
df2 <- df1[,-(12:17),drop=FALSE]
df3 <- df2[,-2,drop=FALSE]
df4 <- df3[,-10, drop=FALSE]
df5<- df4[, -(2:3),drop=FALSE]
#glimpse(df5) #see columns of interest
df66<- df5[, -(5:8),drop=FALSE]
df6<- df66[, -1,drop=FALSE]

###mean and SD's for each column###
DATA <- data.table(df5)
wide <- setnames(DATA[, sapply(.SD, function(x) list(mean=round(mean(x), 8), 
                  sd=round(sd(x), 8)))], c( sapply(names(DATA), paste0, c(".mean", ".SD"))))
wide

###correlation matrix###
#corrplot(cor(df5))
#chart.Correlation(df5, histogram=TRUE, pch=19)
#Distribution and correlation of variables of interest
ggpairs(df,columns=(7:9), aes(color=`Regional indicator`, alpha=0.5),
        upper = list(continuous =wrap("cor", size=3.8)), lower = list(continuous = "points"),
        title = "State Scatterplot Matrix") 

###COMPARE WEST AND CENTRAL-EAST EUROPE###
#boxplot of Logged GDP and Social Support filtered by regional indicator
ggplot(data=df, aes(x=NULL,y=`Logged GDP per capita`, fill=`Regional indicator`)) +
  geom_boxplot(notch = FALSE)
ggplot(data=df, aes(x=NULL,y=`Social support`, fill=`Regional indicator`)) +
  geom_boxplot(notch = FALSE)

#create a subset
cw_eu <- df$`Regional indicator`=="Central and Eastern Europe" 
EU_CE <-df[cw_eu, ]
w_eu <- df$`Regional indicator`=="Western Europe" 
OVEST_EU <-df[w_eu, ]

#Boxplot, mean and standard deviation of variables
mean(OVEST_EU$`Logged GDP per capita`)
sd(OVEST_EU$`Logged GDP per capita`)
mean(EU_CE$`Logged GDP per capita`)
sd(EU_CE$`Logged GDP per capita`)
boxplot(OVEST_EU$`Logged GDP per capita`,
        main = "log(GDP) per capita in W-EU",
        xlab = "Count",
        ylab = "Logged GDP per capita",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE) 
boxplot(EU_CE$`Logged GDP per capita`,
        main = "log(GDP) per capita in CE-EU",
        xlab = "Count",
        ylab = "Logged GDP per capita",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE) 

#qqplot 
qqnorm(EU_CE$`Logged GDP per capita`, main = "Normal qq-plot of log(GDP) in CE-EU", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(EU_CE$`Logged GDP per capita`, col="red")
qqnorm(OVEST_EU$`Logged GDP per capita`, main = "Normal qq-plot of log(GDP) in W-EU", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(OVEST_EU$`Logged GDP per capita`, col="red")

#t.test for logged GDP per capita
perm.t.test(EU_CE$`Logged GDP per capita`,OVEST_EU$`Logged GDP per capita`, var.equal = T)
#results without permutation are better
# t.test for social support
qqnorm(EU_CE$`Social support`, main = "qq-plot Social Support in CE-EU", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(EU_CE$`Social support`, col="red")
qqnorm(OVEST_EU$`Social support`, main = "qq-plot Social Support in W-EU", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(OVEST_EU$`Social support`, col="red")
#p-value>0.05 , so means of two groups are equal
perm.t.test(EU_CE$`Social support`,OVEST_EU$`Social support`)

########Does social support depend on log(GDP)?########
#scatterplot
ggplot(df, aes(x = `Logged GDP per capita`, y = `Social support`)) + 
  geom_point(alpha = .7) + geom_point(aes(color = `Regional indicator`)) +
  labs(title = "Logged GDP per capita", subtitle = "There is linear relation between two variables?") + 
  theme_minimal()
#Pearson correlation
cor.test(df$`Logged GDP per capita`, df$`Social support`)
perm.t.test(df$`Logged GDP per capita`, df$`Social support`)
#not good p-value

#log(GDP) has tipical qq-plot of uniform distribution
qqnorm(df5$`Logged GDP per capita`, main = "Normal qq-plot of log(GDP)", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(df5$`Logged GDP per capita`, col="red")
#Social Support has qq-plot like left talled data
qqnorm(df5$`Social support`, main = "Normal qq-plot of Social Support", ylab = "Empirical Quantiles",
       xlab = "Theoretical Quantiles")
qqline(df5$`Social support`, col="red")

###linear regression with 3 models###
model =lm(`Logged GDP per capita` ~ `Social support` , data = df5)
summary(model)
ggplot(df5, aes(x = `Logged GDP per capita`, y = `Social support`)) +
  geom_point() +
  stat_smooth(method = lm)
plot( model )

model1 =lm(`Logged GDP per capita` ~ `Social support` + `Healthy life expectancy` , data = df5)
summary(model1)
ggplot(df5, aes(x = `Logged GDP per capita`, y = `Social support`+ `Healthy life expectancy`)) +
  geom_point() +
  stat_smooth(method = lm)
plot( model1 )
#qq-plot of uniform data

model2 =lm(`Logged GDP per capita` ~ `Social support`*`Healthy life expectancy` , data = df5)
summary(model2)
ggplot(df5, aes(x = `Logged GDP per capita`, y = `Social support`*`Healthy life expectancy`)) +
  geom_point() +
  stat_smooth(method = lm)
plot( model2 )
#qq-plot of uniform data

###ANOVA with 3 models###
one.way <- aov(`Logged GDP per capita` ~ `Social support` , data = df5)
summary(one.way)
two.way <- aov(`Logged GDP per capita` ~ `Social support`+`Healthy life expectancy`, data = df5)
summary(two.way)
interaction <- aov(`Logged GDP per capita` ~ `Social support`*`Healthy life expectancy`, data = df5)
summary(interaction)
#y=a + bx where a is different from zero in all 3 models.

#compare models
model.set <- list(one.way, two.way, interaction)
model.names <- c("one.way", "two.way", "interaction")
aictab(model.set, modnames = model.names)

###GDP depend on regional indicator?###
model_state <- lm(`Logged GDP per capita`~ `Regional indicator`, data=df)
summary(model_state)
model_state2 <- lm(`Logged GDP per capita`~ `Social support`+`Regional indicator`, data=df)
summary(model_state2)
model_state3 <- lm(`Logged GDP per capita`~ `Social support`*`Regional indicator`, data=df)
summary(model_state3)
model.set <- list(model_state, model_state2, model_state3)
model.names <- c("solo", "sum", "interaction")
aictab(model.set, modnames = model.names)


###PARTIAL CORRELATION-CORRELATION OF RESIDUALS###
cor_GDP_Social <- cor(df5$`Logged GDP per capita`, df5$`Social support` )
cor_GDP_HealthyLife <- cor(df5$`Logged GDP per capita`, df5$`Healthy life expectancy`)
cor_SocialSupport_HealthyLife <- cor(df5$`Social support`, df5$`Healthy life expectancy`)
regress_GDP_Social <- lm(`Logged GDP per capita` ~ `Social support` , data = df5)
residuals_GDP_Social <- residuals(regress_GDP_Social)
regress_GDP_HealtyLife <- lm(`Logged GDP per capita` ~ `Healthy life expectancy` , data = df5)
residuals_GDP_HealtyLife <- residuals(regress_GDP_HealtyLife)
regress_SocialSupport_HealtyLife <- lm(`Social support` ~ `Healthy life expectancy` , data = df5)
residuals_SocialSupport_HealtyLife <- residuals(regress_SocialSupport_HealtyLife)
partial_cor <- cor(residuals_SocialSupport_HealtyLife , residuals_GDP_HealtyLife)
print(partial_cor)
ggplot()+
  geom_point(aes(x=residuals_SocialSupport_HealtyLife, y=residuals_GDP_HealtyLife))+
  labs(title=paste('Correlation = ',round(cor_GDP_Social,2),"\n",
                   'Partial Correlation = ',round(partial_cor,2),sep=""),
       x='Res(Social Support ~ Healty Life expectancy)',
       y= 'Res(Logged GDP per capita ~ Healty Life expectancy)')


###glmnet + cross validation###
A <- model.matrix(`Logged GDP per capita` ~ `Regional indicator`*`Social support`, data=df)
lambda <- 10**seq(6, -4, length = 300)
lm_lasso <- glmnet(A, df$`Logged GDP per capita`, alpha=1, lambda = lambda)
plot(lm_lasso, xvar= "lambda")
lm_ridge <- glmnet(A, df$`Logged GDP per capita`, alpha=0, lambda = lambda)
plot(lm_ridge, xvar= "lambda")
lm_elnet <- glmnet(A, df$`Logged GDP per capita`, alpha=0.5, lambda = lambda)
plot(lm_elnet, xvar= "lambda")

lm_elnet_cv <- cv.glmnet(A,df$`Logged GDP per capita`)
plot(lm_elnet_cv)
lambda = lm_elnet_cv$lambda.1se
print(lambda)
#coefs <- as.matrix(coef(lm_elnet_cv, s = lm_elnet_cv$lambda.1se))
#coefs






