## popisne statistiky vsech kardinalnich promennych
library(dplyr)
library(psych)
library(readxl)
library(epiDisplay)


ukol <- read_excel("uvod do programovani/ukol.xlsx")
deskripceNonRound <- describe(ukol, omit = TRUE, quant=c(.25,.75))
deskripceRounded <- round(deskripceNonRound, 3)
deskripceFinal <- subset(deskripceRounded, select = -c(vars, mad))
tabulka_deskripce <- grid.table(deskripceFinal, rows = c("Útrata", "Spropitné", "Počet hostů"), cols = c("N", "Mean", "SD", "Median", "Trimmed 0.1", "Min", "Max", "Range", "Skew", "Kurtosis", "SE", "1st Q", "3rd Q"))

## distribuce vsech kardinalnich promennych
histUtrata <- hist(ukol$utrata, xlab = "Útrata", main = "Distribution of the Variable 'Útrata'", breaks = 20)
histSpropitne <- hist(ukol$spropitne, xlab = "Spropitné", main = "Distribution of the variable 'Spropitné'", breaks = 20)
histHosti <- hist(ukol$pocet_hostu, xlab = "Počet hostů", main = "Distribution of the Variable 'Počet hostů'", breaks = 20)


## boxploty kardinalnich promennych
boxplot(x = ukol$pocet_hostu, xlab = "Počet hostů", main = "Boxplot of the variable 'Počet hostů'", col = "#7FB3D5")
boxplot(x = ukol$spropitne, xlab = "Spropitné", main = "Boxplot of the variable 'Spropitné'", col = "#7FB3D5")
boxplot(x = ukol$utrata, xlab = "Útrata", main = "Boxplot of the variable 'Útrata'", col = "#7FB3D5")

## barploty nominalnich promennych
barplotPohlavi <- ggplot(ukol) + aes(x = pohlavi, fill = pohlavi) + geom_bar() + ggtitle("Barplot of the Variable 'Pohlaví'")
barplotKurak <- ggplot(ukol) + aes(x = kurak, fill = kurak) + geom_bar() + ggtitle("Barplot of the Variable 'Kuřák'")
barplotDen <- ggplot(ukol) + aes(x = den, fill = den) + geom_bar() + ggtitle("Barplot of the Variable 'Den'")
barplotCas <- ggplot(ukol) + aes(x = cas, fill = cas) + geom_bar() + ggtitle("Barplot of the Variable 'Čas'")

## frekvencni tabulky pro nominalni data
freqPohlavi <- subset(as.data.frame(tab1(ukol$pohlavi, cum.percent = FALSE)), select = -c(first.line))
freqPohlaviTab <-  grid.table(freqPohlavi, rows = c("Muži", "Ženy", "Celkem"), cols = c("Četnost", "Relativní četnost"))

freqKurak <- subset(as.data.frame(tab1(ukol$kurak, cum.percent = FALSE)), select = -c(first.line))
freqKurakTab <-  grid.table(freqKurak, rows = c("Kuřák", "Nekuřák", "Celkem"), cols = c("Četnost", "Relativní četnost"))

freqDen <- subset(as.data.frame(tab1(ukol$den, cum.percent = FALSE)), select = -c(first.line))
freqDenTab <-  grid.table(freqDen, rows = c("Čtvrtek", "Neděle","Pátek", "Sobota", "Celkem"), cols = c("Četnost", "Relativní četnost"))

freqCas <- subset(as.data.frame(tab1(ukol$cas, cum.percent = FALSE)), select = -c(first.line))
freqCasTab <-  grid.table(freqCas, rows = c("Oběd", "Večeře", "Celkem"), cols = c("Četnost", "Relativní četnost"))

## popisna statistika pro utratu podle promenne kurak
UtrataKurak1 <- describeBy(ukol$utrata, group = ukol$kurak, quant=c(.25,.75), mat = TRUE)
UtrataKurak <- subset(UtrataKurak, select = -c(group1))
UtrataKurak <- round(UtrataKurak, 3)
UtrataKurakTab <- grid.table(UtrataKurak2, rows = c("Kuřák", "Nekuřák"),  cols = c("N", "Mean", "SD", "Median", "Trimmed 0.1", "Min", "Max", "Range", "Skew", "Kurtosis", "SE", "1st Q", "3rd Q"))

##vizualizace pro utratu podle promenne kurak
BoxUtrataKurak <- ggplot(data = ukol, aes(x = kurak, y = utrata, fill = kurak)) + geom_boxplot() + geom_jitter() + ggtitle("Boxplot of the variable 'Utrata' by the variable 'Kurak'")
violinUtrataKurak <- ggplot(data = ukol, aes(x = kurak, y = utrata, fill = kurak)) + geom_violin() + ggtitle("Boxplot of the variable 'Utrata' by the variable 'Kurak'")

## popisna statistika pro spropitne podle promenne den
SpropitneDen <- describeBy(ukol$spropitne, group = ukol$den, quant=c(.25,.75), mat = TRUE)
SpropitneDen <- subset(SpropitneDen, select = -c(group1, vars, mad))
SpropitneDen <- round(SpropitneDen, 3)
SpropitneDenTab <- grid.table(SpropitneDen, rows = c("Čtvrtek", "Neděle", "Pátek", "Sobota"),  cols = c("N", "Mean", "SD", "Median", "Trimmed 0.1", "Min", "Max", "Range", "Skew", "Kurtosis", "SE", "1st Q", "3rd Q"))



##vizualizace pro spropitne podle promenne den
BoxSpropitneDen <- ggplot(data = ukol, aes(x = den, y = spropitne, fill = den)) + geom_boxplot() + geom_jitter() + ggtitle("Boxplot of the variable 'Spropitne' by the variable 'Den'")
violinSpropitneDen <- ggplot(data = ukol, aes(x = den, y = spropitne, fill = den)) + geom_violin() + ggtitle("Boxplot of the variable 'Spropitne' by the variable 'Den'")
