# ===============================================================
# ESERCIZIO 1
# ===============================================================

rm(list = ls())

data(mtcars)

# - consumo in miglia per gallone (mpg),
# - numero di cilindri (cyl),
# - cavalli (hp),
# - peso in migliaia di libbre (wt),
# - tipo di trasmissione (am: 0 = automatica, 1 = manuale).

# 1.1
# Studiare la distribuzione della variabile mpg (consumo) 
# calcolando indici di posizione, variabilità e forma.
# Rappresentare graficamente
# la distribuzione mediante un grafico appropriato
# e commentare le principali caratteristiche osservate.

# Indici di posizione
summary(mtcars$mpg)

# Il valore della media (20.09) leggermente più alto di quello 
# della mediana (19.20) ci suggerisce la presenza di un'asimmetria
# positiva nelal distribuzione della variabile mpg.

# Indici di variabilità

# Poichè abbiamo a che fare con un campione, non è necessario
# correggere il valore restituito da var() moltiplicandolo
# per ((n - 1) / n).
var_mpg <- var(mtcars$mpg)
sd_mpg <- sqrt(var_mpg)
cv_mpg <- sd_mpg / mean(mtcars$mpg)

# Il coefficiente di variabilità (~0.29) indica che il consumo
# si discosterà in maniera moderata dalla media.

# Indici di forma
library(labstatR)
skew_mpg <- skew(mtcars$mpg)

# Il valore dell'indice di asimmetria è abbastanza alto (~0.64),
# quindi ci aspettiamo una distribuzione con un'asimmetria
# positiva.

boxplot(mtcars$mpg,
        col = "lightcoral",
        main = "Boxplot consumo in miglia per gallone")

# Il boxplot conferma graficamente quanto detto precedentemente:
# la distribuzione presenta un'asimmetria positiva, lo si evince 
# dalla maggior lunghezza del baffo superiore rispetto
# al baffo interiore. Non si notano particolari outliers.

# 1.2
# Confrontare le distribuzioni del consumo 
# condizionatamente al tipo di trasmissione,
# utilizzando un grafico appropriato. 
# Calcolare le medie condizionate 
# e confrontare la variabilità tra i due gruppi.

# Medie condizionate
medie_cond <- tapply(mtcars$mpg, mtcars$am, mean)

# Le auto a trasmissione automatica (0) percorrono
# mediamente ~17.14 miglia per gallone,
# mentre le auto a trasmissione manuale (1) ne percorrono mediamente ~24.39.
# Dunque le auto a trasmissione automatica consumano più delle auto
# a trasmissione manuale.

# Variabilità
var_cond <- tapply(mtcars$mpg, mtcars$am, var)
sd_cond <- sqrt(var_cond)
cv_cond <- sd_cond / medie_cond

# Le auto a trasmissione automatica presentano una minor
# variabilità nel consumo (~0.223) rispetto alle auto con
# trasmissione manuale (~0.25).

# Grafico
boxplot(mtcars$mpg ~ mtcars$am,
        col = c("lightgreen", "lightblue"),
        main = "Boxplot mpg ~ am",
        xlab = "Tipo di trasmissione",
        ylab = "Consumo (Miles / (US) gallon)")

# Il boxplot conferma quanto detto: le auto a trasmissione automatica
# percorrono mediamente meno miglia per gallone, mentre la auto
# a trasmissione automatica ne percorrono di più. Non emergono
# outliers evidenti.

# 1.3 
# Creare una nuova variabile categoriale peso_cat
# dividendo la variabile wt (peso) in tre classi di pari numerosità.
# Costruire una tabella di frequenza doppia 
# tra peso_cat e cyl (numero di cilindri) 
# e commentare l’eventuale associazione tra le due variabili.

# Trovo i percentili (terzili) che ci interessano utilizzando
# la funzione quantile():
percentili_peso_cat <- quantile(mtcars$wt,
                                probs = c(0, 1/3, 2/3, 1))

# Utilizzo la funzione cut() per creare le classi
# utilizzando i percentili precedentemente trovati:
mtcars$peso_cat <- cut(mtcars$wt,
                       breaks = percentili_peso_cat,
                       include.lowest = TRUE)

# 1.4 
# Analizzare la relazione lineare tra
# il consumo (mpg) e il peso (wt) delle automobili:
# • Rappresentare i dati con un diagramma di dispersione.
# • Calcolare il coefficiente di correlazione di Pearson e 
#   commentarne il valore.
# • Adattare un modello di regressione lineare semplice,
#   interpretare i coefficienti stimati e valutarne la significatività
#   statistica.
# • Commentare la bontà di adattamento del modello e prevedere il
#   consumo atteso per un’automobile che pesa 3 migliaia di libbre

# Per poter analizzare una relazione lineare è innanzitutto
# necessario creare un modello lineare, utilizzando
# come variabili:
# Indipendente (Y): peso (wt);
# Dipendente (X): consumo (mpg).
modello <- lm(mpg ~ wt, data = mtcars)

# Diagramma di dispersione
plot(mtcars$wt, mtcars$mpg,
     pch = 19,
     col = "blue",
     main = "Scatterplot mpg ~ wt",
     xlab = "Peso (1000 lbs)",
     ylab = "Consumo (Miles / (US) gallon)")

abline(modello, col = "red", lwd = 2)

# Dal diagramma di dispersione è possibile notare che vi è
# una relazione lineare negativa tra il consumo e il peso,
# più la macchina sarà pesante e meno miglia per gallone
# percorrerà.

coef_pearson <- cor(mtcars$mpg, mtcars$wt)

# Il coefficiente di Pearson (~ -0.87) conferma l'esistenza di una forte
# correlazione negativa tra le due variabili.

summary(modello)

# Il modello di regressione lineare scelto descrive una forte
# relazione lineare negativa tra le variabili wt ed mpg.
#
# Il coefficiente di wt (-5.3445) indica che, all'aumentare
# di 1000 lbs di peso, le miglia per gallone percorse
# diminuiranno di 5.3445.
# Con un p-value significativo (1.29e-10) che conferma
# l'esistenza di una relazione tra le due variabili.
#
# Retta attesa:
# mpg_atteso = 37.2851 - 5.3445 * wt
#
# Il valore dell'intercetta non ha alcun significato pratico,
# in quanto dice che auto di peso 0 percorrono 37.2851 miglia per gallone.
#
# Il coefficiente di adattamento R-squared = 0.7528 ci dice
# che il modello riesce a spiegare ~75% della variabilità
# delle miglia per gallone percorse al variare del peso.

# Previsione del consumo atteso per un’automobile
# che pesa 3 migliaia di libbre
nuovi_dati <- data.frame(wt = 3)

predict(modello,
        newdata = nuovi_dati,
        interval = "prediction",
        level = 0.95)

# Per un automobile che pesa 3 migliaia di libbre ci aspettiamo
# un consumo di 21.25171 miglia per gallone.

# ===============================================================
# ESERCIZIO 2
# ===============================================================

rm(list = ls())

# Definiamo i seguenti eventi:
# A: servizio di streaming 1
# B: servizio di streaming 2

P_not_A <- 0.25
P_not_B_dato_not_A <- 0.85
P_A_and_not_B <- 0.40

#2.1
# • la probabilità che un individuo si abboni ad (A);

# P(A) = 1 - P(A')
P_A <- 1 - P_not_A

# 2.2  
# • la probabilità che un individuo si abboni ad (A) dato che
# non si è abbonato a (B);

# Si utilizza il teor. di Bayes:
# P(A / B') = P(A ^ B') / P(B')
#
# Calcoliamo P(B'):
# P(B') = P(A ^ B') + P(A' ^ B')
#
# Calcoliamo P(A' ^ B'):
# P(A' ^ B') = P(B' / A') * P(A')

P_not_A_and_not_B <- P_not_B_dato_not_A  * P_not_A
P_not_B <- P_A_and_not_B + P_not_A_and_not_B

P_A_dato_not_B <- P_A_and_not_B / P_not_B

#2.3
# • la probabilità che un individuo si abboni ad (A) oppure a (B).

# Si usa la formula dell'unione:
# P(A U B) = P(A) + P(B) - P(A ^ B )

P_B <- 1 - P_not_B
P_A_and_B <- P_A - P_A_and_not_B

P_A_or_B <- P_A + P_B - P_A_and_B

# ===============================================================
# ESERCIZIO 3
# ===============================================================

rm(list = ls())

data(ToothGrowth)

# 3.1
# Calcolare le medie marginali e condizionate della variabile
# lunghezza dei denti
# 
# 3.2
# Rappresentare graficamente le distribuzioni della lunghezza dei denti
# condizionate ai vari fattori, e costruire un grafico per visualizzare
# l’eventuale presenza di interazione tra tipo di supplemento e dose.

medie_supp <- tapply(ToothGrowth$len, ToothGrowth$supp, mean)
medie_dose <- tapply(ToothGrowth$len, ToothGrowth$dose, mean)

# Dalle medie della lunghezza dei denti condizionate al tipo
# di supplemento ed alla dose si evince che:
# - len ~ supp: in media la lunghezza dei denti dei porcellini
#   d'india a cui è stato somministrato succo d'arancia (OJ) è
#   maggiore rispetto a quelli a cui è stata somministrata
#   vitamina C pura (~20.66 > ~16.96)
#
# - len ~ dose: in media la lunghezza dei denti dei porcellini
#   d'india a cui è stato somministrata una dose maggiore di
#   vitamina C, ha una lunghezza dei denti sempre maggiore,
#   quindi all'aumentare della dose di vitamina C, la lunghezza
#   dei denti aumenterà.

medie_interazione <- tapply(ToothGrowth$len, 
                            list(ToothGrowth$supp, ToothGrowth$dose),
                            mean)

# A basse dosi (0.5 e 1 mg), il succo d’arancia sembra più efficace
# rispetto all’acido ascorbico.
#
# Alla dose più alta (2 mg), entrambi i metodi portano a denti di
# lunghezza simile (~26.1), suggerendo un possibile effetto di interazione
# tra tipo di supplemento e dose.

# Grafico
par(mfrow = c(1, 2))

boxplot(ToothGrowth$len ~ ToothGrowth$supp,
        col = "lightgreen",
        main = "Boxplot len ~ supp",
        xlab = "Supplemento",
        ylab = "Lunghezza")

boxplot(ToothGrowth$len ~ ToothGrowth$dose,
        col = "lightblue",
        main = "Boxplot len ~ dose",
        xlab = "Dose (mg/d)",
        ylab = "Lunghezza")

par(mfrow = c(1, 1))

# Dai boxplot è possibile notare che:
# - len ~ supp: la lunghezza dei denti dei porcellini
#   d'india a cui è stato somministrato succo d'arancia (OJ) è
#   maggiore rispetto a quelli a cui è stata somministrata
#   vitamina C pura.
#
# - len ~ dose: la lunghezza dei denti dei porcellini
#   d'india a cui è stato somministrata una dose maggiore di
#   vitamina C, ha una lunghezza dei denti sempre maggiore,
#   quindi all'aumentare della dose di vitamina C, la lunghezza
#   dei denti aumenterà.

interaction.plot(x.factor = ToothGrowth$dose,
                 trace.factor = ToothGrowth$supp,
                 response = ToothGrowth$len,
                 fun = mean,
                 type = "b",
                 pch = 19,
                 lwd = 2,
                 col = c("lightcoral", "lightblue"),
                 ylab = "Lunghezza media",
                 xlab = "Dose (mg/giorno)",
                 trace.label = "Supplemento")

# Poichè le due rette non sono perfettamente paralelle, possiamo dire
# che vi è una relazione tra la dose ed il tipo di supplemento
# somministrato. Inizialmente, a basse dosi la lunghezza dei denti
# risulta maggiore in soggetti a cui è stato somministrato succo
# d'arancia; a dosi più alte la differenza della lunghezza 
# in relazione al tipo di supplemento è meno marcata.

# 3.3
# Eseguire un’analisi della varianza (ANOVA) a due vie per verificare:
# • l’effetto principale del metodo di somministrazione (supp);
# • l’effetto principale della dose (dose);
# • l’eventuale presenza di interazione tra i due fattori.
# Riportare la tabella ANOVA completa e commentare i risultati ottenuti,
# specificando quali effetti sono statisticamente significativi al
# livello α= 0.05.

ToothGrowth$supp <- factor(ToothGrowth$supp)
ToothGrowth$dose <- factor(ToothGrowth$dose)

modello_aov <- aov(len ~ supp * dose, data = ToothGrowth)
summary(modello_aov)

# L'ANOVA a due vie mostra che sia il tipo di supplemento
# (supp, p-value ~ 0.000231), sia la dose (dose, p-value < 2e-16) hanno
# un effetto significativo sulla lunghezza dei denti.
#
# Anche l'interazione tra supplemento e dose (supp:dose, p-value ~ 0.021860)
# risulta essere significativo, indicando che l'effetto della dose dipende
# dal tipo di supplemento.
#
# CONCLUSIONE: sia gli effetti principali sia l'interazione influenzano
# significativamente la crescita dei denti.

# 3.4
# Se necessario, effettuare il test di Tukey HSD per i confronti multipli
# sui fattori risultati significativi, identificando quali gruppi differiscono
# in modo statisticamente significativo.

tukey <- TukeyHSD(modello_aov,
                  c("supp", "dose"),
                  conf.level = 0.95)
plot(tukey)

# Il test di Tukey mostra differenze significative nei livelli
# medi dell'interazione tra supp e dose, confermando quanto
# dimostrato dall'ANOVA.

# ===============================================================
# ESERCIZIO 4
# ===============================================================

rm(list = ls())

# Campione di osservazioni 
x <- c(992, 1003, 995, 987, 1001, 998, 990, 996, 1005, 991)

# Numero di osservazioni
n <- length(x)        

# Media campionaria
x_bar <- mean(x)      

# Deviazione standard campionaria
s <- sd(x)       

# 4.1
# costruire l’intervallo di confidenza per il parametro µ al
# livello di confidenza 0.95, assumendo σ2 ignota;

# Livello di significatività
alpha <- 1 - 0.95

# Statistica test utilizzata: T-Student con n-1 gradi di libertà
# perchè la varianza sigma2 è ignota:
# T = (x_bar - mu0) / (s / sqrt(n))

# Calcolo il quantile per un intervallo di confidenza
# bilaterale con alpha distribuito in entrambe le code
t_crit <- qt(1 - alpha/2, df= n-1)

# Formula dell'intervallo di confidenza:
# IC = x_bar +/- t_crit * (s / sqrt(n))

IC_lower <- x_bar - t_crit * (s / sqrt(n))
IC_upper <- x_bar + t_crit * (s / sqrt(n))
IC <- c(IC_lower, IC_upper)

# L'intervallo di confidenza ottenuto per il parametro mu0
# indica che quest'ultimo ha un valore compreso tra
# [~991.55, ~1000.05] con un livello di confidenza del 95%.

# 4.2
# verificare il test di ipotesi
# H0 : µ= 1000 vs H1 : µ<1000
# al livello di significatività α = 0.05.
# Calcolare la statistica di test, il p-value e formulare la conclusione.

# Livello di significatività
alpha <- 0.05

# Media teorica
mu0 <- 1000

# Statistica test utilizzata: T-Student con n-1 gradi di libertà
# perchè la varianza sigma2 è ignota:
T_obs <- (x_bar - mu0) / (s / sqrt(n))

# Calcolo del p-value per un test monolaterale sinistro:
p_value <- pt(T_obs, df = n-1)

# Il valore del p-value (~0.03 < 0.05) è significativo,
# rifiuto l'ipotesi nulla H0 poichè ho evidenze statistiche
# per affermare che il parametro mu0 sia minore di 1000.

# Nota:
# L'intervallo di confidenza bilaterale include 1000,
# ma il test unilaterale rifiuta H0.
# Non è un controsenso: l'IC considera deviazioni sopra e sotto la media,
# mentre il test unilaterale guarda solo se la media è
# significativamente più bassa di 1000.

p_value_bilaterale <- 2 * (1 - pt(abs(T_obs), df = n-1))