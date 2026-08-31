# ===============================================================
# ESERCIZIO 1
# ===============================================================

rm(list = ls())

data(airquality)

# Le variabili di interesse sono:
# - concentrazione di ozono (Ozone, in ppb);
# - radiazione solare (Solar.R, in Langleys);
# - velocità del vento (Wind, in mph).
# - temperatura (Temp in gradi Fahrenheit).

# 1.1
# Rimuovere le osservazioni mancanti utilizzando la funzione na.omit().

airquality <- na.omit(airquality)

# 1.2
# Rappresentare graficamente le distribuzioni delle quattro variabili
# (Ozone, Solar.R, Wind Temp) e commentare le principali caratteristiche osservate.
# Indicare quale variabile presenta la maggiore variabilità
# utilizzando un indice appropriato.

# Ozone

# Indici di posizione
summary(airquality$Ozone)

# Il valore della media (42.1) è più alto del valore della mediana (31.5),
# quindi ci aspettiamo un'asimmetria positiva (verso destra) nella
# distribuzione della variabile.

# Indici di variabilità
var_Ozone <- var(airquality$Ozone)
sd_Ozone <- sqrt(var_Ozone)
cv_Ozone <- sd_Ozone / mean(airquality$Ozone)

# Il coefficiente più adeguato da utilizzare è il coefficiente di variabilità
# in quanto è il modo migliore per confrontare le distribuzioni di variabili
# che hanno tipi diversi.
#
# Il valore del coefficiente di variabilità (0.79) è elevato, indicando
# un alto discostamento dei valori dalla loro media.

# Indici di forma (asimmetria)
# Nota: se labstatR non è presente, si può usare skewness() del pacchetto e1071
library(labstatR) 
skew(airquality$Ozone)

# Il valore dell'indice di asimmetria è piuttosto elevato (1.21),
# ciò conferma quanto detto in precedenza: vi è una forte asimmetria positiva.

# Grafico
boxplot(airquality$Ozone,
        col = "gray",
        main = "Boxplot Concentrazione ozono")

# Il boxplot conferma la forte asimmetria positiva, lo si evince
# dalla maggior lunghezza del baffo superiore rispetto al baffo inferiore.
# Inoltre conferma anche la forte variabilità dei dati, infatti si notano
# evidenti outliers.

# Solar.R

# Indici di posizione
summary(airquality$Solar.R)

# Il valore della media (185.9) è più basso del valore della mediana (207),
# quindi ci aspettiamo un'asimmetria negativa (verso sinistra) nella
# distribuzione della variabile.

# Indici di variabilità
var_Solar.R <- var(airquality$Solar.R)
sd_Solar.R <- sqrt(var_Solar.R)
cv_Solar.R <- sd_Solar.R / mean(airquality$Solar.R)

# Il valore del coefficiente di variabilità (0.48) è nella media, indicando
# un discreto discostamento dei valori dalla loro media.

# Indici di forma (asimmetria)
skew(airquality$Solar.R)

# Il valore dell'indice di asimmetria (-0.42),
# conferma quanto detto in precedenza: vi è un'asimmetria negativa.

# Grafico
boxplot(airquality$Solar.R,
        col = "yellow",
        main = "Boxplot Radiazione solare")

# Il boxplot conferma graficamente quanto detto: vi è un'asimmetria negativa,
# lo si evince dalla maggior lunghezza del baffo inferiore rispetto a quello
# superiore.

# Wind

# Indici di posizione
summary(airquality$Wind)

# Il valore della media (9.94) è molto simile al valore della mediana (9.7),
# quindi ci aspettiamo una leggera asimmetria positiva
# nella distribuzione della variabile.

# Indici di variabilità
var_Wind <- var(airquality$Wind)
sd_Wind <- sqrt(var_Wind)
cv_Wind <- sd_Wind / mean(airquality$Wind)

# Il valore del coefficiente di variabilità (0.35) è mediamente basso,
# indicando un discreto discostamento dei valori dalla loro media.

# Indici di forma (asimmetria)
skew(airquality$Wind)

# Il valore dell'indice di asimmetria (0.35),
# conferma quanto detto in precedenza: vi è una lieve asimmetria positiva.

# Grafico
boxplot(airquality$Wind,
        col = "lightblue",
        main = "Boxplot Vento")

# Dal boxplot non si vede un'asimmetria particolarmente marcata, questo
# è dato dal fatto che la media e la mediana hanno valori talmente vicini
# da non causare una particolare asimmetria.
# Si notano alcuni outliers.

# Temp

# Indici di posizione
summary(airquality$Temp)

# Il valore della media (77.88) è inferiore rispetto al valore della mediana (79),
# quindi ci aspettiamo una leggera asimmetria negativa
# nella distribuzione della variabile.

# Indici di variabilità
var_Temp <- var(airquality$Temp)
sd_Temp <- sqrt(var_Temp)
cv_Temp <- sd_Temp / mean(airquality$Temp)

# Il valore del coefficiente di variabilità (0.12) è molto basso,
# indicando un basso discostamento dei valori dalla loro media.

# Indici di forma (asimmetria)
skew(airquality$Temp)

# Il valore dell'indice di asimmetria (-0.37),
# conferma quanto detto in precedenza: vi è una lieve asimmetria negativa.

boxplot(airquality$Temp,
        col = "orange",
        main = "Boxplot Temperatura")

# Dal boxplot non si vede un'asimmetria particolarmente marcata, questo
# è dato dal fatto che la media e la mediana hanno valori talmente vicini
# da non causare una particolare asimmetria.

# 1.3
# Studiare come varia la temperatura media nei diversi mesi (Month).
# Rappresentare graficamente le medie mensili e
# commentare eventuali tendenze stagionali osservate.

# Calcoliamo le medie delle temperature condizionate dai mesi
medie_cond <- tapply(airquality$Temp, airquality$Month, mean)

barplot(medie_cond,
        col = "orange",
        main = "Temperatura media mensile",
        xlab = "Mese",
        ylab = "Temperatura media (Fahrenheit)")

# Dal grafico a barre che rappresenta le medie mensili è possibile
# affermare che le temperature dei mesi 7 (luglio) e 8 (agosto) sono
# le più alte dei mesi presi in considerazione.
# Dal mese 5 (maggio), con temperatura più bassa, notiamo un trend ascendente
# della temperatura, che si ferma ad agosto, in quanto le temperature da
# settembre tendono a scendere nuovamente.

# 1.4
# Ottenere la distribuzione di frequenza doppia
# della variabile Month (mese) e una nuova variabile binaria
# ozono_alto definita come segue:
# valore 1 se Ozone è superiore alla mediana, O altrimenti.
# Calcolare le distribuzioni di frequenze relative di
# questa nuova variabile condizionatamente a Month
# e commentare se esiste una relazione tra il mese
# e i livelli elevati di Ozono.

# Calcolo la mediana di Ozone
mediana_Ozone <- median(airquality$Ozone)

# Creo la nuova variabile ozono_alto all'interno del df airquality
airquality$ozono_alto <- ifelse(airquality$Ozone > mediana_Ozone, 1, 0)

# Creo la tabella di frequenza doppia (contingenza)
freq_assolute <- table(airquality$ozono_alto, airquality$Month)

# Calcolo le frequenze relative condizionate per colonna (mese)
# Usiamo margin = 2 per condizionare al mese
freq_relative <- prop.table(freq_assolute, margin = 2)
print(freq_relative)

# Esiste una relazione tra il mese e i livelli di ozono, infatti:
# Nei mesi estivi centrali (luglio e agosto) si osservano più frequentemente 
# livelli elevati rispetto ai mesi iniziali e finali.

# 1.5
# Studiare la relazione tra concentrazione di ozono (Ozone)
# e velocità del vento (Wind):
# • Rappresentare la relazione mediante un diagramma di dispersione.
# • Calcolare il coefficiente di correlazione di Pearson
# e commentarne segno e intensità.
# • Adattare un modello di regressione lineare semplice
# e interpretare i coefficienti ottenuti.
# • Valutare la significatività statistica dei parametri
# e la bontà globale del modello.
# • Condurre un'analisi grafica dei residui.
# • Prevedere la concentrazione di ozono per una giornata
# con velocità del vento pari a 12

# Adatto un modello di regressione lineare tra le due variabili.
# Variabile indipendente (X): Velocità del vento (Wind).
# Variabile dipendente (Y): Concentrazione di ozono (Ozone).

modello <- lm(Ozone ~ Wind, data = airquality)

# Diagramma di dispersione
plot(airquality$Wind, airquality$Ozone,
     pch = 19,
     col = "blue",
     main = "Scatterplot Wind ~ Ozone",
     xlab = "Velocità del vento (mph)",
     ylab = "Concentrazione di ozono (ppb)")

abline(modello, col = "red", lwd = 2)

# Coefficiente di correlazione di Pearson
coef_pearson <- cor(airquality$Ozone, airquality$Wind)

# Il valore del coefficiente di correlazione di Pearson (~ -0.61)
# indica una discreta relazione lineare negativa tra la velocità del
# vento e la concentrazione di ozono nell'aria.
# 
# Il diagramma di dispersione conferma quanto detto nell'analisi del
# coefficiente di correlazione di Pearson.

summary(modello)

# Il modello di regressione lineare scelto indica una relazione lineare
# negativa tra la concentrazione di ozono e la velocità del vento.
#
# Il coefficiente di Wind (~ -5.55) indica che all'aumentare della velocità 
# del vento di 1 mph, la concentrazione di ozono diminuisce di circa 5.55 ppb.
#
# Il p-value significativo (molto basso, < 2e-16) conferma l'esistenza di una
# relazione tra le due variabili.
#
# Retta attesa:
# Ozone_atteso = 96.87 - 5.55 * Wind
#
# Il valore dell'intercetta (96.87) ha un significato statistico:
# ci dice che se la velocità del vento fosse pari a 0 mph,
# la concentrazione di ozono attesa sarebbe di circa 96.87 ppb.
#
# Il coefficiente di bontà di adattamento R-squared = 0.3752, dice
# che il modello riesce a spiegare circa il 37.5% della variabilità
# della concentrazione di ozono.

# Analisi dei residui

par(mfrow = c(1, 4)) # Visualizziamo 4 grafici insieme

plot(modello)

par(mfrow = c(1, 1))

# Verifica se il modello rispetta le assunzioni del modello lineare.
#
# Residuals vs Fitted: si osserva una struttura non completamente casuale
# (a forma di U) e una dispersione crescente dei residui,
# indice di possibile eteroschedasticità e non-linearità.
#
# Q-Q Residuals (residui): code pesanti; i valori sulle code si distaccano
# dalla retta teorica, indicando che i residui non seguono perfettamente
# una distribuzione normale.

# Previsione della concentrazione di ozono per una giornata
# con velocità del vento pari a 12

# Il dataframe deve contenere la variabile INDIPENDENTE (Wind)
nuovi_dati <- data.frame(Wind = 12)

predict(modello,
        newdata = nuovi_dati,
        interval = "prediction",
        level = 0.95)

# La concentrazione di ozono per una giornata con velocità del vento
# pari a 12 mph, è di ~30.29 ppb.

# ===============================================================
# ESERCIZIO 2
# ===============================================================

rm(list = ls())

# Definiamo i seguenti eventi:
# - lezioni di recupero (evento R)
# - studio autonomo (evento A)
# - tutoraggio individuale (evento T)
# - superare l'esame (evento E)

P_R <- 0.5
P_A <- 0.3
P_T <- 0.2

P_E_dato_R <- 0.8
P_E_dato_A <- 0.5
P_E_dato_T <- 0.9

# 2.1
# Calcolare la probabilità che uno studente scelto a caso superi l'esame.

# Si usa il teorema della probabilità totale:
# P(E) = P(E / R) * P(R) + P(E / A) * P(A) + P(E / T) * P(T)

P_E <- P_E_dato_R * P_R +
  P_E_dato_A * P_A +
  P_E_dato_T * P_T

# 2.2
# Supponendo che uno studente abbia superato l'esame,
# calcolare la probabilità che abbia utilizzato il tutoraggio individuale

# Si usa il teorema di Bayes:
# P(T / E) = P(E ^ T) / P(E)
#
# Troviamo P(E ^ T) da P(E / T)
# P(E ^ T) = P(E / T) * P(T)

P_E_and_T <- P_E_dato_T * P_T

P_T_dato_E <- P_E_and_T / P_E

# ===============================================================
# ESERCIZIO 3
# ===============================================================

rm(list = ls())

data(warpbreaks)

# L'esperimento considera due fattori:
# • breaks: numero di rotture di fili di lana durante la tessitura;
# • wool: tipo di lana utilizzata (A o B);
# • tension: livello di tensione applicata (L = bassa, M = media, H = alta).

# 3.1
# Calcolare le medie del numero di rotture
# per ciascun livello dei due fattori e per ciascuna combinazione dei fattori

# Medie per i singoli fattori
medie_wool <- tapply(warpbreaks$breaks, warpbreaks$wool, mean)
medie_tension <- tapply(warpbreaks$breaks, warpbreaks$tension, mean)

# Medie per le combinazioni
medie_interazione <- tapply(warpbreaks$breaks, 
                            list(warpbreaks$wool, warpbreaks$tension), 
                            mean)

# Dalle medie condizionate emerge che:
# - Wool: Il numero di fili rotti è mediamente maggiore per la lana A (31.0)
#   rispetto alla lana B (25.3).
# - Tension: Le rotture diminuiscono drasticamente all'aumentare
#   della tensione (L=36.4, M=26.4, H=21.7).
# - Interazione: Guardando le combinazioni, si nota che la lana B si comporta
#   in modo strano: a tensione alta (H) ha quasi le stesse rotture
#   della tensione media, mentre la lana A cala molto.
#   Questo suggerisce una possibile interazione.
  
# 3.2
# Rappresentare graficamente le distribuzioni
# del numero di rotture condizionatamente ai due fattori,
# utilizzando un grafico appropriato

par(mfrow = c(1, 2))

boxplot(warpbreaks$breaks ~ warpbreaks$wool,
        col = "white",
        main = "Boxplot breaks ~ wool",
        xlab = "Tipo di lana",
        ylab = "Numero di rotture")

boxplot(warpbreaks$breaks ~ warpbreaks$tension,
        col = "orange",
        main = "Boxplot breaks ~ tension",
        xlab = "Livello di tensione",
        ylab = "Numero di rotture")

par(mfrow = c(1, 1))

# I boxplot confermano quanto detto nell'analisi delle medie condizionate.
# C'è da aggiungere che per quanto riguarda il numero di fili rotti per 
# i tipi di lana A sono presenti degli outliers; stessa cosa per il livello
# di tensione applicata H (alta).

# Un grafico utile per capire l'ANOVA a due vie è l'interaction plot
interaction.plot(warpbreaks$tension, warpbreaks$wool, warpbreaks$breaks,
                 pch = 19,
                 lwd = 2,
                 col = c("lightcoral", "lightblue"),
                 main = "Interaction Plot: Wool vs Tension",
                 xlab = "Tensione",
                 ylab = "Media di rotture")

# L'interaction plot mostra che le linee non sono parallele:
# l'effetto della tensione è diverso a seconda della lana.

# 3.3
# Eseguire un'analisi della varianza (ANOVA) a due vie per verificare:
# • l'effetto principale del tipo di lana (wool);
# • l'effetto principale del livello di tensione (tension);
# • l'eventuale presenza di interazione tra i due fattori.
# Riportare la tabella ANOVA completa e commentare dettagliatamente
# i risultati ottenuti al livello di significatività a = 0.05.

# Creo il modello
modello_aov <- aov(breaks ~ wool * tension, data = warpbreaks)

summary(modello_aov)

# L'ANOVA a due vie mostra che il tipo di lana
# (wool, p-value ~ 0.058213 > 0.05) non ha un effetto significativo
# sulla rottura dei fili di lana durante la tessitura,
# mentre la tensione (tension, p-value ~ 0.0007 << 0.05) risulta essere
# significativa nella rottura dei fili.
#
# L'interazione tra tipo di lana e tensione (wool:tension, p-value ~ 0.021)
# risulta essere significativa, indicando che la tensione applicata al filo
# che si rompe dipende anche dal tipo di lana.
#
# CONCLUSIONE: l'unico effetto principale che influenza significativamente
# la rottura dei fili di lana è la tensione; mentre l'interazione 
# ha un effetto significativo.

# 3.4
# Qualora appropriato, applicare il test di Tukey HSD
# per i confronti identificando quali gruppi o
# combinazioni differiscono in modo significativo.

tukey <- TukeyHSD(modello_aov, conf.level = 0.95)

plot(tukey)

# Secondo il test di Tukey:
# - Se l'intervallo NON include 0: differenza significativa
# - Se l'intervallo INCLUDE 0: differenza non significativa

# Il test di Tukey evidenza delle differenze significative
# nell'interazione tra la tensione ed il tipo di lana, confermando
# i risultati della tabella ANOVA.

# ===============================================================
# ESERCIZIO 4
# ===============================================================

rm(list = ls())

# Campione osservato:
x <- c(36.5, 37.2, 36.8, 37.5, 36.9, 37.0,
       36.6, 36.7, 37.1, 36.4, 36.3, 36.8)

# Numero di osservazioni
n <- length(x)

# Media campionaria
x_bar <- mean(x)

# 4.1  
# Supponendo che la varianza sia nota e pari a sigma2 = 2,
# costruire un intervallo di confidenza al 95% per il parametro u.

# Varianza nota
sigma2 <- 2
sigma <- sqrt(sigma2)

# Livello di significatività
alpha <- 1- 0.95

# Statistica test utilizzata: Normale perchè la varianza sigma2 è nota:
# Z = (x_bar - mu0) / (sigma / sqrt(n))

# Calcolo del quantile per un intervallo di confidenza bilaterale
# con alpha distribuito in entrambe le code
z_crit <- qnorm(1 - alpha/2)

# Formula dell'intervallo di confidenza per il parametro mu0:
# IC = x_bar +/- z_crit * (sigma / sqrt(n))

IC_lower <- x_bar - z_crit * (sigma / sqrt(n))
IC_upper <- x_bar + z_crit * (sigma / sqrt(n))

IC <- c(IC_lower, IC_upper)

# Dall'intervallo di confidenza ottenuto possiamo notare che
# il parametro mu0 ha un valore compreso tra [36.01651, 37.61682]
# con un livello di confidenza del 95%.

# 4.2
# Nello stesso contesto con varianza nota,
# condurre un test d'ipotesi al livello di significatività a = 0.05 per:
# H0: mu0 = 37 vs H1: mu0 < 37

# Livello di significatività
alpha <- 0.05

# Media teorica
mu0 <- 37

# Poichè la varianza è nota, utilizzo sempre la normale:
Z_obs <- (x_bar - mu0) / (sigma / sqrt(n))

# Metodo del p-value:
p_value <- pnorm(Z_obs)

# Poichè il p-value ha un valore di circa ~0.33 >> 0.05, non ho
# evidenze statistiche per rifiutare l'ipotesi nulla H0, quindi
# non possiamo affermare che la temperatura media interna dei sensori
# sia minore di 37°C

# 4.3
# Ora si assuma che la varianza sia ignota.

# 4.3.a
# Costruire l'intervallo di confidenza al 95% per mu0
# utilizzando la deviazione standard campionaria.

# Deviazione standard campionaria
s <- sd(x)

# Statistica test utilizzata: T-Student con n-1 gradi di libertà
# perchè la varianza sigma2 è ignota:
# T = (x_bar - mu0) / (s / sqrt(n))

# Calcolo del quantile per un intervallo di confidenza bilaterale
# con alpha distribuito in entrambe le code
t_crit <- qt(1 - alpha/2, df = n-1)

# Formula dell'intervallo di confidenza per il parametro mu0:
# IC = x_bar +/- t_crit * (s / sqrt(n))

IC_lower <- x_bar - t_crit * (s / sqrt(n))
IC_upper <- x_bar + t_crit * (s / sqrt(n))

IC <- c(IC_lower, IC_upper)

# Dall'intervallo di confidenza ottenuto possiamo notare che
# il parametro mu0 ha un valore compreso tra [36.59518, 37.03815]
# con un livello di confidenza del 95%.

# 4.3.b 
# Ripetere il test di ipotesi del punto (b)
# (Ho : mu0 = 37 vs H1 : mu0 < 37)
# usando l'approccio corretto per varianza ignota.

# Poichè la varianza è ignota, utilizzo sempre la T-Student
# con n-1 gradi di libertà:
t.test(x,
       mu = 37,
       alternative = "less",
       conf.level = 0.95)

# Poichè il p-value ha un valore di circa ~0.048 < 0.05, quindi ho
# evidenze statistiche per per rifiutare l'ipotesi nulla H0, quindi
# possiamo affermare che la temperatura media interna dei sensori
# sia minore di 37°C

# 4.3.c
# Confrontare i risultati ottenuti con quelli del test con varianza nota:
# • Quale dei due intervalli di confidenza è più ampio? Motivare la differenza.
# • Il risultato del test cambia? Confrontare i risultati.

# L'intervallo di confidenza con varianza nota è molto più ampio
# non a causa della distribuzione, ma perché la
# deviazione standard teorica imposta sigma ~ 1.41 è molto più grande
# di quella osservata nel campione s ~ 0.35.
#
# Il test cambia esito perché l'assunzione sigma2 = 2 era
# troppo pessimistica rispetto alla precisione reale dei sensori.
# Usando la varianza campionaria, abbiamo ridotto l'errore standard,
# rendendo il test più sensibile e portandoci a rifiutare H0.
# Questo dimostra che usare una varianza teorica errata può portare
# a conclusioni sbagliate.