#? rendre vendredi 22 d?c avant 00h

# install.packages("zoo")
# install.packages("svglite")
library(ggplot2)
library(zoo)
library(dplyr)
library(gridExtra)
library(svglite)
library(forecast)
library(tseries)



### T?l?chargement ###
data_eol <- read.table("EolUSA01-22 (2).txt")
data_eol <- ts(data_eol, frequency=12, start=c(2001,1))
length(data_eol)

### Representation graphique ###
plot(data_eol, main="Production d'?lectricit? ?olienne aux Etats-Unis", 
     xlab="Ann?e", 
     ylab="Production en m?gawatt/heure", lwd=1.2)
# on voit bien que c'est une s?rie multiplicative


##### df_eol ####
df_eol = data.frame(y=as.matrix(data_eol), date=time(data_eol))
#df_eol$date <- as.yearmon(df_eol$date)



  ##### Graph. eol ----
ggplot(data = df_eol, aes(x = date, y = V1)) +
  geom_line(col = "#1F363D") +
  labs(x = "Année", y = "Production en MWh", 
       title = "Production éléctrique des éoliennes aux États-Unies",
       subtitle = "(en mégawatt/heure)") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        axis.title.x = element_text(size = 15L))




#### Transformation en s?rie additive ####
eol_log <- log(data_eol)
eol_log <- ts(eol_log, frequency=12, start=c(2001,1))
plot(eol_log, main="Production d'?lectricit? ?olienne aux Etats-Unis
     logarithme du nombre mensuel (en milliers de m?gawatt/heure)", 
     xlab="Ann?e", 
     ylab="Production")


#### df_eol_log ####
df_eol_log = data.frame(y=as.matrix(eol_log), date=time(eol_log))
#### Graph. eol log ----
ggplot(data = df_eol_log, aes(x = date, y = V1)) +
  geom_line(col = "#1F363D") +
  labs(x = "Année", y = "Production", 
       title = "Production des éoliennes aux États-Unis",
       subtitle = "logarithme de la production mensuelle (en milliers de mégawatt/heure)") +
  theme_classic() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(face = "italic", hjust = 0.5), 
        axis.title.x = element_text(size = 15L))




#### df_tend ####

### tendence ###
y <- as.vector(eol_log)
x <- as.vector(time(eol_log))

xclass <- cut(x,22)
meananual <- tapply(y,xclass, mean)
t <- seq(2001.5, 2022.5, by=1)

plot(x,y, pch=".", main="Production d'?lectricit? ?olienne aux Etats-Unis
     logarithme du nombre mensuel (en milliers de m?gawatt/heure)", ylab="", xlab="ann?e", cex.main=1.2, cex.lab=1.2)
lines(x,y,lwd=1.2)
lines(t,meananual, col="red",lwd=2)
legend("topleft", legend=c("donn?es", "courbe de regression"), text.col=c("black","red"))

#### Graph. Tendance ----
df_tend = data.frame(x, y, t, meananual)
ggplot(df_tend, aes(x = x, y = y)) +
  geom_line(size = 0.5, aes(color = "Données")) +
  #geom_point(shape = 19, size = 1.25, aes(color = "Données")) +
  geom_line(aes(x = t, y = meananual, color = "Courbe de régression"), size = 1.25) +
  labs(x = "Année", y = "Production", 
       title = "Production des éoliennes aux États-Unis",
       subtitle = "logarithme de la production mensuelle (en milliers de mégawatt/heure)",
       caption = "Données et courbe de régression") +
  scale_color_manual(values = c("Données" = "#1F363D", "Courbe de régression" = "#9116c9")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2001, 2024),
                     breaks = seq(2001, 2024, by = 2))



#### MMC12 ####

  #### comparaison entre la courbe de r?gression avec MMC12 ####
eol_mmc <- stats::filter(eol_log, filter = c(1/24, rep(1/12, 11), 1/24))


plot(eol_log, main="Production d'?lectricit? ?olienne log", ylab="Production", xlab="Ann?e")
lines(t,meananual, col="red")
lines(x,eol_mmc,col="blue")
legend("topleft", legend=c("donn?es", "courbe de regression", "MMC12"), text.col=c("black","red", "blue"))


df_eol_mmc = data.frame(x, eol_log, t, meananual, eol_mmc)

#### Graph. MMC12/Regression log----
ggplot(df_eol_mmc, aes(x = x)) +
  geom_line(aes(y = eol_log, color = "Données")) +
  geom_line(aes(x = t, y = meananual, color = "Courbe de régression"), size = 0.75) +
  geom_line(aes(x = x, y = eol_mmc, color = "MMC12"), size = 0.75) +
  labs(
    x = "Année",
    y = "Production",
    title = "Production électrique des éoliennes aux États-Unis",
    subtitle = "logarithme de la production mensuelle (en milliers de mégawatt/heure)",
    caption = "Données, courbe de régression, courbe MMC12"
  ) +
  scale_color_manual(values = c("Données" = "#1F363D", "Courbe de régression" = "#9116c9", "MMC12" = "#c91640")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2001, 2024),
                     breaks = seq(2001, 2024, by = 2))




#### Graph. MMC12/Regression log----
ggplot(df_eol_mmc, aes(x = x)) +
  geom_line(aes(y = exp(eol_log), color = "Données")) +
  geom_line(aes(x = t, y = exp(meananual), color = "Courbe de régression"), size = 0.75) +
  geom_line(aes(x = x, y = exp(eol_mmc), color = "MMC12"), size = 0.75) +
  labs(
    x = "Année",
    y = "Production",
    title = "Production électrique des éoliennes aux États-Unis",
    subtitle = "production mensuelle (en milliers de mégawatt/heure)",
    caption = "Données, courbe de régression, courbe MMC12"
  ) +
  scale_color_manual(values = c("Données" = "#1F363D", "Courbe de régression" = "#9116c9", "MMC12" = "#c91640")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2001, 2024),
                     breaks = seq(2001, 2024, by = 2)) +
  scale_y_continuous(limits = c(0, 50000),
                     breaks = seq(0, 50000, by = 10000))




### ? voir s'il faut faire sans log (demander au prof??) ###
plot(data_eol, main="Production d'?lectricit? ?olienne", 
     xlab="ann?e", 
     ylab="",lwd=1.2)
lines(t,exp(meananual), col="red", lwd=2)
lines(x,exp(eol_mmc),col="blue", lwd=2)
legend("topleft", legend=c("donn?es", "courbe de regression", "MMC12"), text.col=c("black","red", "blue"))



#### df_seasonal ####

### Q2. Coef + d?composition
dec <- decompose(eol_log, type="additive")
plot(dec)

df_observ <- data.frame(y = as.matrix(dec$x), date = time(dec$x))
df_seasonal <- data.frame(y = as.matrix(dec$seasonal), date = time(dec$seasonal))
df_trend <- data.frame(y = as.matrix(dec$trend), date = time(dec$trend))
df_random <- data.frame(y = as.matrix(dec$random), date = time(dec$random))

Go = ggplot(df_observ, aes(x = date, y = V1), color="#1F363D") +
  geom_line() +
  labs(
    x = "Année",
    y = "Valeurs",
    title = "Série Observée",
    subtitle = "",
    caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))


# Créer un graphique pour le composant saisonnier
Gs = ggplot(df_seasonal, aes(x = date, y = y), color="#1F363D") +
  geom_line() +
  labs(
    x = "Année",
    y = "Valeurs",
    title = "Composant Saisonnier",
    subtitle = "",
    caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))


# Créer un graphique pour le composant de tendance
Gt = ggplot(df_trend, aes(x = date, y = y), color="#1F363D") +
  geom_line() +
  labs(
    x = "Année",
    y = "Valeurs",
    title = "Composant de Tendance",
    subtitle = "",
    caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))


# Créer un graphique pour le composant aléatoire
Gr = ggplot(df_random, aes(x = date, y = x...seasonal), color="#1F363D") +
  geom_line() +
  labs(
    x = "Année",
    y = "Valeurs",
    title = "Composant Aléatoire",
    subtitle = "",
    caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))



#svglite("~/Desktop/BUT SD/SAE/Série Temporelle /Graph/Series_decomposition.svg")
grid.arrange(Go,Gt,Gs,Gr, nrow=2)
#dev.off()


plot(exp(dec$trend))
plot(exp(dec$seasonal), xlim=c(2001,2022), main="coefficients saisonniers",
     xlab="",ylab="")


df_seasonal = data.frame(y=as.matrix(exp(dec$seasonal)), date=time(dec$seasonal))
  #### Graph. Coef. Saison (vérifier ort) ----
ggplot(df_seasonal, aes(x = date)) +
  geom_line(aes(y = y, color = "Coefficient")) +
  labs(
    x = "Année",
    y = "Coefficient",
    title = "Coefficients saisonniers",
    subtitle = "de 2001 à 2022",
    caption = " "
  ) +
  scale_color_manual(values = c("Coefficient" = "#1F363D")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 13L),
    axis.title.y = element_text(size = 13L),
    legend.position = "none"
  )



df_seasonal$date <- as.Date(time(df_seasonal$date))

  #### Graph. Coef. Saison 1an ----
ggplot(df_seasonal, aes(x = date)) +
  geom_line(aes(y = y, color = "Coefficient"), size=2) +
  labs(
    x = "Mois",
    y = "Coefficient",
    title = "Coefficients saisonniers",
    subtitle = "sur une année",
    caption = " "
  ) +
  scale_color_manual(values = c("Coefficient" = "#1F363D")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 13L),
    axis.title.y = element_text(size = 13L),
    legend.position = "none"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  coord_cartesian(xlim = as.Date(c("2011-01-01", "2011-12-01")))



#### Séries ajustées ####

### Q3. S?ries ajust?es
plot(data_eol, main="Production d'?lectricit? ?olienne", 
     xlab="ann?e", 
     ylab="",lwd=1.2)
lines(x, exp(dec$seasonal+dec$trend), col = "red")
legend("topleft", legend=c("donn?es", "s?rie ajust?e"), text.col=c("black","red"))
plot(log(data_eol), main="Production d'?lectricit? ?olienne", 
     xlab="ann?e", 
     ylab="",lwd=1.2)
lines(x, dec$seasonal+dec$trend, col = "red")
legend("topleft", legend=c("donn?es", "s?rie ajust?e"), text.col=c("black","red"))


df_eol_ajust = data.frame(x, data_eol, exp(dec$seasonal+dec$trend))
  #### Graph. Série ajustée ----
ggplot(df_eol_ajust, aes(x = x)) +
  geom_line(aes(y = V1, color = "Données")) +
  geom_line(aes(x = x, y = dec.trend, color = "Série ajustée"), size = 0.60) +
  labs(
    x = "Année",
    y = "Production",
    title = "Production électrique des éoliennes aux États-Unis",
    subtitle = "(en mégawatt/heure)",
    caption = "Données, série ajustée"
  ) +
  scale_color_manual(values = c("Données" = "#1F363D", "Série ajustée" = "#cf4868")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2001, 2024),
                     breaks = seq(2001, 2024, by = 2))




#### Résidus ####

#boxplot des r?sidus
boxplot(exp(dec$random))

data = data.frame(exp(dec$random))
  #### Graph. Répartition Résidus ----
ggplot(data, aes(x = x...seasonal , y = "")) +
  geom_boxplot(fill = "#be96fa") +
  labs(
    x = "",
    y = "",
    title = "Répartition des résidus",
    #subtitle = "",
    #caption = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    #axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))


#### Prévisions ####

### Q4. Pr?visions

####pr?vision tendance + saison ####
dec <- decompose(eol_log,type="additive")
CVS <- eol_log-dec$seasonal
plot(CVS)
CVSend = tail(CVS, n = 36)
plot(CVSend, main="CVS tail")
y <- as.vector(CVSend)
x <- as.vector(time(CVSend))
reg <- lm(y~x)
ajust <- reg$coefficients[1]+reg$coefficients[2]*x
lines(ajust~x)
t <- seq(2023, 2024, by=1/12)
y1 <- reg$coefficients[1]+reg$coefficients[2]*t
z <- ts(dec$seasonal, start=c(2021,1), end=c(2022,1),frequency=12)
s <- as.vector(z)

  #### Graph. CVS tail ----
plot(CVSend, main="CVS tail")
lines(ajust~x)
df_CVSend = data.frame(y=as.matrix(CVSend), date=time(CVSend))

ggplot(df_CVSend, aes(x = date, y = eol_log)) +
  geom_line(aes(color="CVS"), size=1.25) +
  geom_line(aes(x = date, y = ajust, color="Tendance"), size=1 ) +
  labs(
    x = "Année",
    y = "Valeur",
    title = "CVS",
    subtitle = "(pour les 3 dernières années seulement)",
    caption = "troncature du CVS intégral"
  ) +
  scale_color_manual(values = c("CVS" = "#1F363D", "Tendance" = "#cf4868")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))



  #### 1 ####

plot(eol_log, main="Pr?vision du log de la production mensuel d'?lectricit?", cex.main=1.2,
     xlab="ann?e", ylab="", xlim=c(2020, 2024), ylim = c(8, 16), lwd=2)
lines(x=t,y=y1+s,col="purple",lwd=2)
legend("topleft", legend=c("donn?es", "prevision Trend + season"),
       text.col=c("black",  "purple"), cex=1.1)

# pour les donn?es r??ls
plot(data_eol, main="Pr?vision de la production mensuel d'?lectricit?", cex.main=1.2,
     xlab="ann?e", ylab="", xlim=c(2001, 2024), ylim=c(0,100000), lwd=2)
lines(exp(y1+s)~t,col="purple",lwd=2)
lines(exp(ajust)~x)
legend("topleft", legend=c("donn?es", "prevision Trend + season"),
       text.col=c("black",  "purple"), cex=1.1)






#### Graph. Forecast 1 ----
eol_forecast_1 = data.frame(date = t, forecast_1 = exp(y1 + s))
df_eol_combined = bind_rows(df_eol, eol_forecast_1)

ggplot(df_eol_combined, aes(x = date, y = V1, color = "Données réelles")) +
  geom_line(size = 1.1) +
  geom_line(aes(x = date, y = forecast_1, color = "Prévision Trend + Season"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Prévision de la production électrique des éoliennes aux États-Unis",
    subtitle = "(en mégawatt/heure)",
    caption = "Données, Prévision Trend + Season"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision Trend + Season" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2019, 2024),
                     breaks = seq(2019, 2024, by = 1)) +
  scale_y_continuous(limits = c(15000, 60000),
                     breaks = seq(15000, 60000, by = 5000))




  #### 2 ####


##pr?vision Holt-Winters
eol_log.hw<-HoltWinters(eol_log)
predict(eol_log.hw, n.ahead=25)
plot(eol_log, main="Pr?vision du log de la production mensuel d'?lectricit?", 
     xlab="ann?e", ylab="", xlim=c(2020,2024), ylim=c(8, 16), lwd=2, cex.main=1.2)
lines(predict(eol_log.hw, n.ahead=25), col=2,lwd=2)
legend("topleft", legend=c("donn?es", "pr?vision Holt Winters"),
       text.col=c("black",  "red"), cex=1.1)



# pour les donn?es r??ls
plot(data_eol, main="Pr?vision de la production mensuel d'?lectricit?", 
     xlab="ann?e", ylab="", xlim=c(2001,2024), ylim = c(0, 60000), lwd=2, cex.main=1.2)
lines(exp(predict(eol_log.hw, n.ahead=24)), col=2,lwd=2)
legend("topleft", legend=c("donn?es", "pr?vision Holt Winters"),
       text.col=c("black",  "red"), cex=1.1)



#rm(df_eol_combined)
eol_log.hw<-HoltWinters(eol_log)

  #### Graph. Forecast 2 ----
eol_forecast_2 = data.frame(date = t, forecast_2 = as.numeric(exp(predict(eol_log.hw, n.ahead = 13))))
df_eol_combined = left_join(df_eol_combined, eol_forecast_2, by = "date")

ggplot(df_eol_combined, aes(x = date, y = V1, color = "Données réelles")) +
  geom_line(size = 1.1) +
  geom_line(aes(x = date, y = forecast_2, color = "Prévision Holt Winters"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Prévision de la production électrique des éoliennes aux États-Unis",
    subtitle = "(en mégawatt/heure)",
    caption = "Données, Prévision Holt Winters"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision Holt Winters" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2019, 2024),
                     breaks = seq(2019, 2024, by = 1)) +
  scale_y_continuous(limits = c(15000, 60000),
                     breaks = seq(15000, 60000, by = 5000))


  #### 3 ####

#Stationnarisation
y_arma=diff(eol_log,lag=12,differences=1)
x_arma=diff(y_arma,lag=1,differences=1)
plot(y_arma)
plot(x_arma)
mean(x_arma)

  ##### Graph. Stationnarisation Arma ----
df_x_arma = data.frame(y=as.matrix(x_arma), date=time(x_arma))
ggplot(df_x_arma, aes(x = date, y = V1)) +
  geom_line(col="#1F363D", size=0.5) +
  geom_hline(aes(yintercept=mean(x_arma), color="Moyenne ≈  -0.00298"), linetype="dashed", size=1) +
  labs(
    x = "Année",
    y = "Valeur",
    title = "Stationnarisation ",
    subtitle = "(deuxième itération)",
    caption = ""
  ) +
  scale_color_manual(values = c("Moyenne ≈  -0.00298" = "red")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))



adf.test(eol_log, alternative=c("stationary"),12)
adf.test(x_arma, alternative=c("stationary"),12)


#Autocorrélations
acf(x_arma, lag.max=24, main="ACF") # q=1 Q=1
pacf(x_arma, lag.max=24, main="PACF") # p=1,2,3 P=2

#### Graph. ACF ARMA ----
acfPlot = ggAcf(x_arma, col=c("#1F363D"), lwd=1)
acfPlot +
  labs(
    x = "Lag",
    y = "ACF",
    title = "Graphique de décision ACF",
    subtitle = "(la première barre a été masquée car ignorée pour la prise de décision)",
    caption = "Prévision ARMA"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  annotate("rect", xmin = 1 - 0.5, xmax = 1 + 0.5,
           ymin = -0.425, ymax = 0, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = 12 - 0.5, xmax = 12 + 0.5,
           ymin = -0.52, ymax = 0, fill = "red", alpha = 0.3)


#### Graph. PACF ARMA ----
pacfPlot = ggPacf(x_arma, col=c("#1F363D"), lwd=1)
pacfPlot +
  labs(
    x = "Lag",
    y = "PACF",
    title = "Graphique de décision PACF",
    subtitle = "",
    caption = "Prévision ARMA"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  annotate("rect", xmin = 4 - 0.5, xmax = 4 + 0.5,
           ymin = -0.16, ymax = 0, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = 24 - 0.5, xmax = 24 + 0.5,
           ymin = -0.185, ymax = 0, fill = "red", alpha = 0.3)




#modèle ARIMA
#z1<-arima(eol_log, order=c(5,1,5), seasonal=list(order=c(2,1,1), period=12))
#z1
#tsdiag(z1, gof.lag=25)

#z2<-arima(eol_log, order=c(5,1,5), seasonal=list(order=c(1,1,1), period=12))
#z2
#tsdiag(z2, gof.lag=25)

#z3<-arima(eol_log, order=c(5,1,4), seasonal=list(order=c(1,1,1), period=12))
#z3
#tsdiag(z3, gof.lag=25)

z4<-arima(eol_log, order=c(5,1,3), seasonal=list(order=c(1,1,1), period=12))
z4
tsdiag(z4, gof.lag=25)



plot(z4$residuals)
shapiro.test(z4$residuals)
hist(z4$residuals,main="Histogramme des résidus",freq=F, col="yellow", ylim=c(0,6))
curve(dnorm(x,mean(z4$residuals),sqrt(var(z4$residuals))),add=TRUE)


prev<-predict(z4, n.ahead=13)
plot(eol_log, main="Prévision du taux de CO2 sur 2 ans", cex.main=1.2,
     xlab="année", ylab="taux de CO2", xlim=c(2018,2024), ylim=c(9.5,12), lwd=2)
lines(prev$pred, col="blue",lwd=2)
legend("topleft", legend=c("données", "prévision ARMA"),
       text.col=c("black",  "blue"), cex=1.2)

#### Graph. Forecast 3 ----
eol_forecast_3 = data.frame(forecast_3=as.matrix(exp(prev$pred)), date=time(prev$pred))
df_eol_combined = left_join(df_eol_combined, eol_forecast_3, by = "date")

ggplot(df_eol_combined, aes(x = date, y = V1, color = "Données réelles")) +
  geom_line(size = 0.6) +
  geom_line(aes(x=date, y=forecast_3, color = "Prévision ARMA"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production (en mégawatt/heure)",
    title = "Prévision de la production des éoliennes aux États-Unis",
    subtitle = "modèle ARMA",
    caption = "Données, Prévision ARMA"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision ARMA" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2019, 2024),
                     breaks = seq(2019, 2024, by = 1)) +
  scale_y_continuous(limits = c(15000, 60000),
                     breaks = seq(15000, 60000, by = 5000))



  ##### Graph. Compar forecast 1,2,3 ----
ggplot(df_eol_combined) +
  geom_line(aes(x = date, y = V1, color = "Données réelles"), size = 1.1) +
  geom_line(aes(x=date, y=forecast_1, color = "Trend + Season"), size = 1.1) +
  geom_line(aes(x=date, y=forecast_2, color = "Holt Winters"), size = 1.1) +
  geom_line(aes(x=date, y=forecast_3, color = "ARMA"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Comparaison des différents modèles de prédiction",
    subtitle = "de la production des éoliennes en 2023 (en mégawatt/heure)",
    caption = ""
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Trend + Season" = "#FFC54C", "Holt Winters" = "#FF4681", "ARMA" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2021, 2024),
                     breaks = seq(2021, 2024, by = 1)) +
  scale_y_continuous(limits = c(15000, 60000),
                     breaks = seq(15000, 60000, by = 5000))





plot(data_eol, main="Prévision du taux de CO2 sur 2 ans", cex.main=1.2,
     xlab="année", ylab="taux de CO2", xlim=c(2021,2024), ylim=c(15000,60000), lwd=2)
lines(exp(prev$pred), col="blue",lwd=2)
lines(exp(predict(eol_log.hw, n.ahead=13)),lwd=2, col="red")
lines(exp(y1+s)~t,col="purple",lwd=2)
legend("topleft", legend=c("données", "prévision ARMA" ),
       text.col=c("black",  "blue"), cex=1.2)



  #### 1(bis) ####

eol_log_bis = head(eol_log, n =length(eol_log)-12)
length(eol_log_bis)

dec <- decompose(eol_log_bis,type="additive")
CVS <- eol_log_bis-dec$seasonal
plot(CVS)
y <- as.vector(CVS)
x <- as.vector(time(CVS))
CVSend = tail(CVS, n=36)
plot(CVSend)
y <- as.vector(CVSend)
x <- as.vector(time(CVSend))
reg <- lm(y~x)
ajust <- reg$coefficients[1]+reg$coefficients[2]*x
lines(ajust~x)
t <- seq(2022, 2023, by=1/12)
y1 <- reg$coefficients[1]+reg$coefficients[2]*t
z <- ts(dec$seasonal, start=c(2020,1), end=c(2021,1),frequency=12)
s <- as.vector(z)


plot(eol_log, main="Pr?vision du log de la production mensuel d'?lectricit?", cex.main=1.2,
     xlab="ann?e", ylab="", xlim=c(2020, 2024), ylim = c(8, 16), lwd=2)
lines(y1+s~t,col="purple",lwd=2)
legend("topleft", legend=c("donn?es", "prevision Trend + season"),
       text.col=c("black",  "purple"), cex=1.1)

plot(data_eol, main="Pr?vision de la production mensuel d'?lectricit?", cex.main=1.2,
     xlab="ann?e", ylab="", xlim=c(2001, 2024), ylim=c(0,100000), lwd=2)
lines(exp(y1+s)~t,col="purple",lwd=2)
#lines(exp(ajust)~x)
legend("topleft", legend=c("donn?es", "prevision Trend + season"),
       text.col=c("black",  "purple"), cex=1.1)


res = residuals(reg, type="response")
EQM = data.frame(EQM_1_bis = sqrt(sum(res^2)/length(res)))




#ggplot(df_eol, aes(x = date, y = V1)) +
#  geom_line() +
#  geom_line(aes(x=t, y=exp(y1+s)))
  

df_eol = data.frame(y=as.matrix(data_eol), date=time(data_eol))
eol_forecast_1_bis = data.frame(date = t, forecast_1_bis = exp(y1 + s))


ggplot() +
  geom_line(aes(x = df_eol$date, y = df_eol$V1, color = "Données réelles"),size = 1.1) +
  geom_line(aes(x = eol_forecast_1_bis$date, y = eol_forecast_1_bis$forecast_1_bis, color = "Prévision Trend + Season"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Pévision de la production électrique des éoliennes aux États-Unis",
    subtitle = "(en mégawatt/heure)",
    caption = "Données, Prévision Trend + Season"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision Trend + Season" = "#FFC54C")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2019, 2023),
                     breaks = seq(2019, 2023, by = 1)) +
  scale_y_continuous(limits = c(10000, 60000),
                     breaks = seq(10000, 60000, by = 10000))





  #### 2(bis) ####

eol_log.hw<-HoltWinters(eol_log)
eol_forecast_2bis = data.frame(date = t, forecast_2_bis = as.numeric(exp(predict(eol_log.hw, n.ahead = 13))))

ggplot() +
  geom_line(aes(x = df_eol$date, y = df_eol$V1, color = "Données réelles"),size = 1.1) +
  geom_line(aes(x = eol_forecast_2bis$date, y = eol_forecast_2bis$forecast_2_bis, color = "Prévision Holt Winters"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Prévision de la production électrique des éoliennes aux États-Unis",
    subtitle = "(en mégawatt/heure)",
    caption = "Données, Prévision Holt Winters"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision Holt Winters" = "#FF4681")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2018, 2023),
                     breaks = seq(2018, 2023, by = 1)) +
  scale_y_continuous(limits = c(15000, 50000),
                     breaks = seq(15000, 50000, by = 5000))



eol_log_bis.hw<-HoltWinters(eol_log_bis)
predict(eol_log_bis.hw, n.ahead=12)
plot(eol_log, main="Pr?vision du log de la production mensuel d'?lectricit?", 
     xlab="ann?e", ylab="", xlim=c(2020,2023), ylim=c(8, 16), lwd=2, cex.main=1.2)
lines(predict(eol_log_bis.hw, n.ahead=12), col=2,lwd=2)
legend("topleft", legend=c("donn?es", "pr?vision Holt Winters"),
       text.col=c("black",  "red"), cex=1.1)



# pour les donn?es r??ls
plot(data_eol, main="Pr?vision de la production mensuel d'?lectricit?", 
     xlab="ann?e", ylab="", xlim=c(2019,2023), ylim = c(0, 60000), lwd=2, cex.main=1.2)
lines(exp(predict(eol_log_bis.hw, n.ahead=12)), col=2,lwd=2.5)
legend("topleft", legend=c("donn?es", "pr?vision Holt Winters"),
       text.col=c("black",  "red"), cex=1.1)


res = residuals(eol_log_bis.hw, type="response")
# EQM = data.frame(EQM_2_bis = sqrt(sum(res^2)/length(res)))
EQM = cross_join(EQM, data.frame(EQM_2_bis = sqrt(sum(res^2)/length(res))))

  #### 3(bis) ####

#Stationnarisation
y_arma_bis=diff(eol_log_bis,lag=12,differences=1)
x_arma_bis=diff(y_arma_bis,lag=1,differences=1)
plot(y_arma_bis)
plot(x_arma_bis)
mean(x_arma_bis)
adf.test(eol_log_bis, alternative=c("stationary"),12)
adf.test(x_arma_bis, alternative=c("stationary"),12)


#Autocorrélations
acf(x_arma_bis, lag.max=24) # q=1 Q=1
pacf(x_arma_bis, lag.max=24) # p=1,2,3 P=2

#modèle ARIMA
z4_bis<-arima(eol_log_bis, order=c(5,1,3), seasonal=list(order=c(1,1,1), period=12))
z4_bis
tsdiag(z4_bis, gof.lag=25)

plot(z4_bis$residuals)
shapiro.test(z4_bis$residuals)
hist(z4_bis$residuals,main="Histogramme des résidus",freq=F, col="yellow", ylim=c(0,6))
curve(dnorm(x,mean(z4_bis$residuals),sqrt(var(z4_bis$residuals))),add=TRUE)
prev<-predict(z4_bis, n.ahead=13)


  ##### Graph. Stationnarisation Arma ----
df_x_arma_bis = data.frame(y=as.matrix(x_arma_bis), date=time(x_arma_bis))
ggplot(df_x_arma_bis, aes(x = date, y = V1)) +
  geom_line(col="#1F363D", size=0.5) +
  geom_hline(aes(yintercept=mean(x_arma_bis), color="Moyenne ≈  -0.00215"), linetype="dashed", size=1) +
  labs(
    x = "Année",
    y = "Valeur",
    title = "Stationnarisation ",
    subtitle = "(deuxième itération)",
    caption = ""
  ) +
  scale_color_manual(values = c("Moyenne ≈  -0.00215" = "red")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = ""))


  #### Graph. ACF ARMA ----
acfPlot_bis = ggAcf(x_arma_bis, col=c("#1F363D"), lwd=1)
acfPlot_bis +
  labs(
    x = "Lag",
    y = "ACF",
    title = "Graphique de décision ACF",
    subtitle = "(la première barre a été masquée car ignorée pour la prise de décision)",
    caption = "Prévision ARMA"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  annotate("rect", xmin = 1 - 0.5, xmax = 1 + 0.5,
           ymin = -0.425, ymax = 0, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = 12 - 0.5, xmax = 12 + 0.5,
           ymin = -0.51, ymax = 0, fill = "red", alpha = 0.3)



   #### Graph. PACF ARMA ----
pacfPlot_bis = ggPacf(x_arma_bis, col=c("#1F363D"), lwd=1)
pacfPlot_bis +
  labs(
    x = "Lag",
    y = "PACF",
    title = "Graphique de décision PACF",
    subtitle = "",
    caption = "Prévision ARMA"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 12L),
    axis.title.y = element_text(size = 12L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  annotate("rect", xmin = 4 - 0.5, xmax = 4 + 0.5,
           ymin = -0.147, ymax = 0, fill = "red", alpha = 0.3) +
  annotate("rect", xmin = 24 - 0.5, xmax = 24 + 0.5,
           ymin = -0.167, ymax = 0, fill = "red", alpha = 0.3)


#### Graph. Forecast 3 ----
eol_forecast_3_bis = data.frame(forecast_3=as.matrix(exp(prev$pred)), date=time(prev$pred))

ggplot() +
  geom_line(aes(x = df_eol$date, y = df_eol$V1, color = "Données réelles"),size = 0.6) +
  geom_line(aes(x=eol_forecast_3_bis$date, y=eol_forecast_3_bis$forecast_3, color = "Prévision ARMA"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production (en mégawatt/heure)",
    title = "Prévision de la production des éoliennes aux États-Unis",
    subtitle = "modèle ARMA",
    caption = "Données, Prévision ARMA"
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Prévision ARMA" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2020, 2023),
                     breaks = seq(2020, 2023, by = 1)) +
  scale_y_continuous(limits = c(15000, 55000),
                     breaks = seq(15000, 55000, by = 5000))



res = residuals(z4_bis, type="response")
EQM = cross_join(EQM, data.frame(EQM_3_bis = sqrt(sum(res^2)/length(res))))
View(EQM)

##### Graph. Compar forecast 1,2,3 ----
ggplot() +
  geom_line(aes(x =df_eol$date, y =df_eol$V1, color = "Données réelles"), size = 1.1) +
  geom_line(aes(x=eol_forecast_1_bis$date, y=eol_forecast_1_bis$forecast_1_bis, color = "Trend + Season"), size = 1.1) +
  geom_line(aes(x=eol_forecast_2bis$date, y=eol_forecast_2bis$forecast_2_bis, color = "Holt Winters"), size = 1.1) +
  geom_line(aes(x=eol_forecast_3_bis$date, y=eol_forecast_3_bis$forecast_3, color = "ARMA"), size = 1.1) +
  labs(
    x = "Année",
    y = "Production",
    title = "Comparaison des différents modèles de prédiction",
    subtitle = "de la production des éoliennes en 2023 (en mégawatt/heure)",
    caption = ""
  ) +
  scale_color_manual(values = c("Données réelles" = "#1F363D", "Trend + Season" = "#FFC54C", "Holt Winters" = "#FF4681", "ARMA" = "purple")) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    axis.title.x = element_text(size = 15L),
    legend.position = "top"
  ) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(limits = c(2020, 2023),
                     breaks = seq(2020, 2023, by = 1)) +
  scale_y_continuous(limits = c(15000, 55000),
                     breaks = seq(15000, 55000, by = 5000))

#svglite("~/Desktop/BUT SD/SAE/Série Temporelle /Graph/Prev_compar_2.svg")
#dev.off()

