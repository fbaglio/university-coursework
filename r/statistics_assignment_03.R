# ===============================================================
# ESERCIZIO 1
# ===============================================================

rm(list = ls())

library(MASS)
data(Pima.tr)

n <- length(Pima.tr[,1])

# Variabili presenti nel dataset:
# - npreg (numero di gravidanze),
# - glu (concentrazione plasmatica di glucosio),
# - bp (pressione arteriosa diastolica, mmHg), 
# - skin (spessore della plica cutanea del tricipite, mm),
# - bmi (indice di massa corporea),
# - ped (funzione pedigree del diabete),
# - age (età, anni),
# - type (presenza di diabete: Yes/No).

# 1.1
# Rappresentare graficamente le distribuzioni delle variabili
# bmi e glu mediante opportuni grafici e commentarne
# le principali caratteristiche.

# bmi

# Indici di posizione
summary(Pima.tr$bmi)

# Il valore della mediana (32.8) è leggermente più alto del valore
# della media (32.31), poichè la differenza è minima, ci aspettiamo una
# distribuzione piuttosto simmetrica della variabile esaminata.

# Indici di variabilità

# Poichè abbiamo a che fare con un campione, utilizzo la 
# varianza campionaria, ovvero il valore restituito dalla
# funzione var(), dunque non correggo moltiplicando per ((n - 1) / n)
var_bmi <- var(Pima.tr$bmi)
sd_bmi <- sqrt(var_bmi)
cv_bmi <- sd_bmi / mean(Pima.tr$bmi)

# Il coefficiente di variabilità non è particolarmente alto (~0.19), ciò
# indica che i valori non tendono a discostarsi particolarmente
# dalla media.

# Indici di forma (asimmetria)
library(labstatR)
skew(Pima.tr$bmi)

# Il valore dell'indice di asimmetria (~0.025) è molto basso, ciò
# conferma quanto detto prima: l'asimmetria è lievissima.

# Grafico
boxplot(Pima.tr$bmi,
        col = "lightgreen",
        main = "Boxplot bmi")

# Il boxplot conferma visivamente quanto detto: l'asimmetria è talmente
# lieve da considerarsi trascurabile. Inoltre non si notano particolari
# outliers.

# glu

# Indici di posizione
summary(Pima.tr$glu)

# Il valore della mediana (120.5) è più basso del valore
# della media (124), ci aspettiamo un'asimmetria positiva
# nella distribuzione della variabile esaminata.

# Indici di variabilità
var_glu <- var(Pima.tr$glu)
sd_glu <- sqrt(var_glu)
cv_glu <- sd_glu / mean(Pima.tr$glu)

# Il coefficiente di variabilità non è particolarmente alto (~0.25), ciò
# indica che i valori non tendono a discostarsi particolarmente
# dalla media.

# Indici di forma (asimmetria)
library(labstatR)
skew(Pima.tr$glu)

# Il valore dell'indice di asimmetria (~0.45) è nella media, ci
# aspettiamo una leggera asimmetria positiva.

# Grafico
boxplot(Pima.tr$glu,
        col = "lightblue",
        main = "Boxplot glu")

# Il boxplot conferma visivamente quanto detto: vi è una leggera asimmetria 
# positiva (la si può notare dalla lunghezza lievemente maggiore del
# baffo superiore rispetto al baffo inferiore). Non si notano
# particolari outliers.

# 1.2
# Scegliere una variabile quantitativa e suddividerla
# in quattro classi di uguale numerosità.
# Riportare gli intervalli delle classi ottenute
# e commentarne l'interpretazione.

# Una variabile quantitativa è una variabile che assume valori
# numerici ed è misurabile. Scelgo la variabile bmi.

# Calcolo i percentili della variabile:
percentili_bmi <- quantile(Pima.tr$bmi,
                           probs = seq(0, 1, 0.25))

# Creo le classi di uguale numerosità:
classi_bmi <- cut(Pima.tr$bmi,
                  breaks = percentili_bmi,
                  include.lowest = TRUE)

table(classi_bmi)

# Le classi ottenute suddividono in quattro gruppi di uguale numerosità
# (circa il 25% delle osservazioni).
# La prima classe ([18.2,27.6]) rappresenta circa il 25% degli individui con
# bmi più basso, mentre l'ultima classe ((36.5,47.9]) rappresenta
# circa il 25% degli individui con bmi più alto.
# Questa suddivisione permette di confrontare gruppi
# di uguale numerosità e analizzare eventuali differenze
# rispetto ad altre variabili.

# 1.3
# Considerando la variabile qualitativa type:
# • costruire la tabella delle frequenze assolute e relative;
# • calcolare un opportuno indice di eterogeneità e commentarne il valore.

freq_ass <- table(Pima.tr$type)
freq_rel <- freq_ass / n

# Dalle tabelle delle frequenze si nota che:
# NO: è il valore più frequente in assoluto (132), che
# corrisponde al 66% dei valori assunti da type;
# Yes: è il valore meno frequente in assoluto (68), che
# corrisponde al 34% dei valori assunti da type.

# L'indice di eterogeneità più appropriato è l'indice di Gini:
gini <- 1 - sum(freq_rel ^ 2)

# Massimo teorico indice di Gini:
# 1 - 1/k, k modalità della variabile (in questo caso k = 2).

# L’indice di eterogeneità vale 0.4488, valore prossimo al massimo teorico (0.5).
# Ciò indica che la distribuzione della variabile type presenta
# un elevato grado di eterogeneità, con le due modalità
# entrambe ben rappresentate, sebbene prevalga la modalità “No”.

# 1.4
# Confrontare la distribuzione della variabile glu condizionatamente a type,
# utilizzando opportuni indici descrittivi
# e una rappresentazione grafica adeguata.
# Commentare le differenze osservate.

# Indici di posizione
media_cond <- tapply(Pima.tr$glu, Pima.tr$type, mean)
mediana_cond <- tapply(Pima.tr$glu, Pima.tr$type, median)

# Dalla media condizionata si osserva che
# la concentrazione plasmatica di glucosio è mediamente
# più bassa nei soggetti senza diabete (~113.11) rispetto
# ai soggetti con diabete (~145.06).
# Anche le mediane confermano tale differenza,
# evidenziando uno spostamento verso valori più elevati nel gruppo “Yes”.

# La media risulta leggermente superiore alla mediana,
# suggerendo una moderata asimmetria positiva della distribuzione.

# Indici di variabilità
var_cond <- tapply(Pima.tr$glu, Pima.tr$type, var)
sd_cond <- sqrt(var_cond)
cv_cond <- sd_cond / media_cond

# I coefficienti di variabilità sono piuttosto bassi (~0.23 e ~0.21),
# quindi i valori non tendono a discostarsi particolamente dalla loro
# media.

# Indici di asimmetria
skew_cond <- tapply(Pima.tr$glu, Pima.tr$type, skew)

# L'indice di asimmetria relativo al "No" è piuttosto alto (~0.622), quindi ci
# aspettiamo un'asimmetria positiva. Mentre per il "Yes" il valore è molto
# basso (~0.018), quindi ci aspettiamo una distribuzione piuttosto
# simmetrica.

# Grafico
boxplot(Pima.tr$glu ~ Pima.tr$type,
        col = "lightblue",
        main = "Boxplot glu ~ type",
        xlab = "Presenza di diabete",
        ylab = "Concentrazione di glucosio")

# Il boxplot conferma visivamente quanto emerso dagli indici:
# il box relativo al gruppo 'Yes' è posizionato più
# in alto rispetto al gruppo 'No', indicando una concentrazione
# di glucosio superiore.
# Nel box relativo al gruppo 'No' si notano degli outlier che
# spiegano l'indice di asimmetria più alto rispetto a quello
# dell'altro gruppo

# 1.5 
# Analizzare la relazione tra le variabili glu e bmi:
# • rappresentare la relazione mediante un diagramma di dispersione;
# • calcolare il coefficiente di correlazione di Pearson
#   e commentarne segno e intensità;
# • adattare un modello di regressione lineare semplice,
#   interpretando i coefficienti stimati;
# • valutare la significatività statistica dei parametri
#   e la bontà complessiva del modello; 
# • condurre un'analisi dei residui;

# Adattiamo un modello di regressione lineare, scegliendo come
# variabile indipendente (X) il bmi, mentre come variabile
# dipendente (Y) la concentrazione di glucosio:
modello <- lm(glu ~ bmi, data = Pima.tr)

# Diagramma di dispersione
plot(Pima.tr$bmi, Pima.tr$glu,
     pch = 19,
     col = "blue",
     main = "Scatterplot glu ~ bmi",
     xlab = "Indice di massa corporea",
     ylab = "Concentrazione di glucosio")

abline(modello, col = "red", lwd = 2)

# Coefficiente di correlazione di Pearson
coef_pearson <- cor(Pima.tr$glu, Pima.tr$bmi)

# Il coefficiente di correlazione di Pearson e il diagramma di dispersione
# ci dicono che vi è una scarsa correlazione tra la concentrazione di glucosio
# e l'indice di massa corporea: coef_pearson = ~0.22.

summary(modello)

# Il modello di regressione lineare ci dice che vi è una scarsa 
# correlazione tra la concentrazione di glucosio e l'indice di massa corporea
# (come già detto in precedenza nell'analisi del coef. di Pearson).
#
# Il coefficiente di bmi (1.1199), ci dice che all'aumentare di 1 unità
# di bmi, la concentrazione di glucosio aumenta di ~1.12.
# Il p-value significativo (~0.002 < 0.05) conferma l'esistenza
# di una relazione tra le due variabili.
#
# Retta attesa:
# glu_atteso = 87.7865 + 1.1199 * bmi
#
# Il valore dell'intercetta non ha un significato pratico perchè
# indica che per soggetti con indice di massa corporea nulla,
# la concentrazione di glucosio sarà di 87.7865, il che ovviamente
# non è possibile.
#
# Il coefficiente di adattamento R-squared = 0.047 
# il modello conferma la debolezza della relazione lineare già
# evidenziata dal coefficiente di Pearson.

# Analisi dei residui
par(mfrow = c(1, 4))
plot(modello)
par(mfrow = c(1, 1))

# Grafico Residuals vs Fitted (Linearità e Omoschedasticità)
# Questo grafico permette di verificare se la relazione è realmente lineare.
# I residui appaiono distribuiti in modo abbastanza casuale attorno alla
# linea dello zero, senza mostrare particolari pattern (come forme a "U"
# o a "imbuto"). Ciò suggerisce che l'ipotesi di linearità sia valida e 
# che la varianza dei residui sia costate (omoschedasticità).
#
# Grafico Normal Q-Q (Normalità dei residui)
# Questo grafico serve a verificare se gli errori seguono una distribuzione 
# normale. Si nota che la maggior parte dei punti giace sulla retta 
# tratteggiata, tuttavia vi sono degli scostamenti evidenti nelle "code" 
# (soprattutto per i valori alti). 
# Questo indica che la distribuzione dei residui presenta una leggera 
# asimmetria e non è perfettamente normale, probabilmente a causa 
# della presenza di outlier già evidenziati nel boxplot del glucosio.
#
# In conclusione, nonostante la significatività del modello, l'analisi 
# dei residui e il basso valore di R-squared (~0.047) confermano che 
# il BMI non è un predittore sufficiente per spiegare interamente 
# la variabilità della concentrazione di glucosio.

# ===============================================================
# ESERCIZIO 2
# ===============================================================

rm(list = ls())

# Definiamo i seguenti eventi:
# A: acquistare un dolcetto;
# B: acquistare un caffè.

P_A <- 0.65
P_B_dato_A <- 0.75
P_not_A_and_not_B <- 0.10

# 2.1
# la probabilità che un cliente acquisti 
# sia un caffè che un dolcetto;

# Dal teorema di Bayes:
# P(B / A) = P(A ^ B) / P(A)
# P(A ^ B) = P(B / A) * P(A)

P_A_and_B <- P_B_dato_A * P_A

# 2.2
# la probabilità che un cliente acquisti
# un dolcetto dato che ha acquistato un caffè;

# Si utilizza il teorema di Bayes:
# P(A / B) = P(A ^ B) / P(B)
#
# Troviamo P(B)
# P(A') = 1 - P(A)
# P(A' ^ B) = P(A') - P(A' ^ B')
# P(B) = P(A ^ B) + P(A' ^ B)

P_not_A <- 1 - P_A
P_not_A_and_B <- P_not_A - P_not_A_and_not_B
P_B <- P_A_and_B + P_not_A_and_B

P_A_dato_B <- P_A_and_B / P_B

# 2.3
# la probabilità che un cliente
# non acquisti un caffè dato che non ha acquistato un dolcetto.

# Si utilizza il teorema di Bayes:
# P(B' / A') = P(A' ^ B') / P(A')

P_not_B_dato_not_A <- P_not_A_and_not_B / P_not_A

# ===============================================================
# ESERCIZIO 3
# ===============================================================

rm(list = ls())

library(MASS)
data(Pima.tr)

# Si definisca una nuova variabile npreg_cat
# ottenuta suddividendo la variabile npreg (numero di gravidanze)
# in tre classi: basso, medio e alto.
# Si richiede di analizzare la variabile glu in funzione
# di due fattori: type (Yes/No) e npreg_cat.

# Trovo i tre percentili della variabile npreg
percentili_npreg <- quantile(Pima.tr$npreg,
                             probs = c(0, 1/3, 2/3, 1))

# Inserisco le classi ottenute tramite la funzione cut()
# all'interno del df Pima.tr
Pima.tr$npreg_cat <- cut(Pima.tr$npreg,
                         breaks = percentili_npreg,
                         include.lowest = TRUE)

# 3.1
# Calcolare le medie di glu per ciascun livello dei
# due fattori e per ciascuna combinazione dei fattori

# Medie condizionate per ciascun fattore
medie_type <- tapply(Pima.tr$glu, Pima.tr$type, mean)
medie_npreg_cat <- tapply(Pima.tr$glu, Pima.tr$npreg_cat, mean)

# Dalle medie condizionate dai fattori si evince che:
# - glu ~ type: in media, la concentrazione di glucosio nel sangue
#   è maggiore in soggetti che hanno il diabete (~145.06), rispetto
#   ai soggetti che non lo hanno (~113.11);
# - glu ~ npreg_cat: in media, la concentrazione di glucosio nel sangue
#   è maggiore nelle classi in cui il numero di gravidanze è più elevato,
#   suggerendo un trend positivo tra il numero di gravidanze e la
#   quantità di glucosio.

# Medie delle interazioni
medie_interazione <- tapply(Pima.tr$glu,
                            list(Pima.tr$type, Pima.tr$npreg_cat),
                            mean)

# Dalle medie delle interazioni è possibile notare che:
# - Soggetti con diabete: hanno mediamente una concentrazione di
#   glucosio più bassa ed è possibile notare un trend ascendente
#   della concentrazione di glucosio all'aumentare del numero di
#   gravidanze;
# - Soggetti senza diabete: hanno mediamente una concentrazione di 
#   glucosio più alta ed è possibile notare un trend discendente
#   della concentrazione di glucosio all'aumentare del numero di
#   gravidanze.

# 3.2
# Rappresentare graficamente le distribuzioni di glu
# condizionate ai due fattori,
# utilizzando un grafico appropriato (ad esempio boxplot).

par(mfrow = c(1, 2))

boxplot(Pima.tr$glu ~ Pima.tr$type,
        col = "lightblue",
        main = "Boxplot glu ~ type",
        xlab = "Presenza di diabete",
        ylab = "Concentrazione di glucosio")

# Il box relativo al gruppo 'Yes' è posizionato più
# in alto rispetto al gruppo 'No', indicando una concentrazione
# di glucosio superiore.
# Nel box relativo al gruppo 'No' si notano degli outlier.

boxplot(Pima.tr$glu ~ Pima.tr$npreg_cat,
        col = "lightgreen",
        main = "Boxplot glu ~ npreg_cat",
        xlab = "Numero di gravidanze (classi)",
        ylab = "Concentrazione di glucosio")

par(mfrow = c(1, 1))

# 3.3
# Eseguire un'analisi della varianza (ANOVA) a due vie per verificare:
# • l'effetto principale della variabile type;
# • l'effetto principale della variabile npreg cat;
# • l'eventuale presenza di interazione tra i due fattori.
# Riportare la tabella ANOVA completa e commentare
# dettagliatamente i risultati ottenuti
# al livello di significatività a = 0.05.

# Creo un modello per fare un'analisi della varianza a due vie:
modello_aov <- aov(glu ~ type * npreg_cat, data = Pima.tr)

summary(modello_aov)

# Il modello dell'analisi della varianza (ANOVA) a due vie
# ad un livello di significatività alpha = 0.05, ci dice che:
# 
# - type (p-value = 7.08e-13 << 0.05): l'effetto della variabile
#   type (quindi se il soggetto soffre di diabete) risulta essere
#   significativa nella concentrazione di glucosio presente nel sangue;
# 
# - npreg_cat (p-value = 0.738 >> 0.05): l'effetto della variabile
#   npreg_cat non risulta essere significativa nella concentrazione di
#   glucosio presente nel sangue;
# 
# - type:npreg_cat (p-value = 0.141 >> 0.05): l'effetto dell'interazione
#   tra le variabili type e npreg_cat non risulta essere significativa
#   nella concentrazione di glucosio presente nel sangue.
#
# CONCLUSIONE: l'unico fattore che influenza significativamente la
# concentrazione di glucosio nel sangue è il soffrire o meno di diabete.

# 3.4 
# Qualora appropriato, applicare il test di Tukey HSD
# per i confronti multipli, identificando quali gruppi
# differiscono in modo significativo.

tukey <- TukeyHSD(modello_aov, conf.level = 0.95)
plot(tukey)

# Secondo il test di Tukey:
# - Se l'intervallo NON include 0: differenza significativa
# - Se l'intervallo INCLUDE 0: differenza non significativa

# Il grafico del test di Tukey mostra gli intervalli di confidenza 
# per le differenze tra le medie dei gruppi.
# 
# Per la variabile 'type' (Yes-No): L'intervallo NON contiene lo zero 
# (è tutto positivo o tutto negativo a seconda dell'ordine), confermando 
# che la differenza tra diabetici e non diabetici è statisticamente significativa.
#
# Per la variabile 'npreg_cat': Tutti gli intervalli di confidenza per i 
# confronti a coppie (es. medio-basso, alto-medio) CONTENGONO lo zero.
# Questo conferma quanto visto nell'ANOVA: non c'è evidenza statistica 
# che il numero di gravidanze (categorizzato) influenzi significativamente 
# il livello di glucosio, al netto del diabete.

# ===============================================================
# ESERCIZIO 4
# ===============================================================

rm(list = ls())

# Campione osservato
x <- c(78.5, 81.2, 79.8, 82.0, 80.5, 79.9,
       80.1, 79.7, 81.0, 78.8, 79.5, 80.3)

# Numero di osservazioni
n <- length(x)

# Media campionaria
x_bar <- mean(x)

# Deviazione standard campionaria
s <- sd(x)

# 4.1
# Testare al livello di significatività a = 0.05
# l'ipotesi Ho: mu = 80 vs H1: mu > 80

# Livello di significatività
alpha <- 0.05

# Media teorica
mu0 <- 80

# Statistica test utilizzata: T-Student con n-1 gradi di libertà
# poichè la varianza sigma2 è ignota:
t.test(x,
       mu = mu0,
       alternative = "greater")

# Poichè il valore del p-value ~ 0.36 >> 0.05, non ho evidenze statistiche
# per rifiutare l'ipotesi nulla H0, non vi sono evidenze statistiche
# per concludere che la media μ sia maggiore di 80

# 4.2
# Costruire un intervallo di confidenza al 95% per la media mu

# Livello di significatività
alpha = 1 - 0.95

# Statistica test utilizata: T-Student con n-1 gradi di libertà
# poichè la varianza sigma2 è ignota

# Trovo il quantile per un intervallo di confidenza bilaterale
# con alpha distribuito in entrambe le code:

t_crit <- qt(1 - alpha/2, df = n-1)

# Formula dell'intervallo di confidenza:
# IC = x_bar +/- t_crit * (s / sqrt(n))

IC_lower <- x_bar - t_crit * (s / sqrt(n))
IC_upper <- x_bar + t_crit * (s / sqrt(n))

IC <- c(IC_lower, IC_upper)

# Dall'intervallo di confidenza ottenuto possiamo affermare che
# il parametro mu0 sia contenuto all'interno dell'intervallo
# [79.48083, 80.73583] con un livello di confidenza del 95%.

# 4.3
# Costruire un intervallo di confidenza al 95% per la varianza sigma2
# e per la deviazione standard sigma.

# Livello di significatività
alpha <- 0.05

# Varianza campionaria
s2 <- var(x)

# Statistica test utilizzata: Chi-quadro: si usa per
# descrivere il comportamento della varianza.
# Chi-quadro = (n - 1) * s2 / sigma2

# Quantili 
chi_lower <- qchisq(alpha/2, df = n-1)
chi_upper <- qchisq(1 - alpha/2, df = n-1)

# Formula dell'intervallo di confidenza per la varianza:
# IC = [(n - 1) * s2 / chi_upper, (n - 1) * s2 / chi_lower]

IC_var_lower <- (n - 1) * s2 / chi_upper
IC_var_upper <- (n - 1) * s2 / chi_lower

IC_var <- c(IC_var_lower, IC_var_upper)

# Dall'intervallo di confidenza ottenuto possiamo affermare che
# il parametro sigma2 sia contenuto all'interno dell'intervallo
# [~0.49, ~2.81] con un livello di confidenza del 95%.

# Intervallo per la deviazione standard
IC_sd <- sqrt(IC_var)

# Dall'intervallo di confidenza ottenuto possiamo affermare che
# il parametro sigma sia contenuto all'interno dell'intervallo
# [~0.69, ~1.68] con un livello di confidenza del 95%.