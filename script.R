# INTRODUCTION #################################################################
# STOCK MARKET BY ALBERTO FRISON
# R PROJECT TO ANALYSE HISTORICAL MARKET DATA
# DONE WITH ♥ BY ALBERTO FRISON
################################################################################


# LIBRARIES AND INITIALIZATION #################################################
library (tidyverse)
library(quantmod)
library(scales)


rm (list = ls()) # cleans up objects in the environment
################################################################################


# DATA WRANGLIND AND CLEANING###################################################

getSymbols(Symbols = c("^GSPC"), src = "yahoo", from = "1900-01-01", to = Sys.Date())
getSymbols(Symbols = c("^SP500TR"), src = "yahoo", from = "1900-01-01", to = Sys.Date()) # TOTAL RETURN INDEX


# CONVERTING INTO A DATAFRAME
df_GSPC <- data.frame (Date = index(GSPC), coredata(GSPC))
colnames(df_GSPC) <- c ("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df_SP500TR <- data.frame(Date = index (SP500TR), coredata (SP500TR))
colnames(df_SP500TR) <- c ("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# df_binded <- inner_join (df_SP500TR, df_GSPC, by = "Date", suffix = c("TR", "IN"))


# SELECTING THE FIRST ENTRY OF THE HISTORICAL DATA TO EVENTUALLY PERFORM AN INDEXING CALCULATION
# first_Date <- min(df_GSPC$Date)
# first_Close <- df_GSPC$Close [df_GSPC$Date == first_Date]
# 
# df_GSPC <- df_GSPC %>%
#   mutate (Close_Index = Close / first_Close * 100) %>%
#   select (Date, Close, Close_Index)



# ATTENTION HERE WE HAVE:
# 1. THE INDEX SINCE 1928 BUT WITH NO DIVIDENDS REINVESTED
# 2. THE INDEX TOTAL RETURN, BUT JUST FROM 1988

# ADDING DAILY RETURNS (SETTING THE DAY TO 0)
df_GSPC$Daily_Returns <- c(0, (df_GSPC$Close[-1] / df_GSPC$Close[-nrow(df_GSPC)] - 1)*100)
df_SP500TR$Daily_Returns <- c(0, (df_SP500TR$Close[-1] / df_SP500TR$Close[-nrow(df_SP500TR)] - 1)*100)

head (df_GSPC)
summary (df_GSPC)

head (df_SP500TR)
summary (df_SP500TR)


################################################################################
# SOME STATISTICAL ANALYSIS
# DAILY RETURNS

# CHOOSE HERE THE INDEX YOU WANT
returns <- df_GSPC$Daily_Returns
returns <- df_SP500TR$Daily_Returns

# LET'S CALCULATE THE MEAN AND THE STANDAR DEVIATION OF THE DAILY RETURNS
mu <- mean(returns, na.rm = T)
sigma <- sd(returns, na.rm = T)

mu 
sigma

# LET'S VISUALIZE TWO THINGS
# 1. THE DISTRIBUTION OF RETURNS
# 2. A NORMAL DISTRIBUTIONS, HAVING MU AND SIGMA AS PARAMETERS

ggplot(data.frame(returns), aes(x = returns)) +
  geom_histogram(aes(y = after_stat(density)), bins = 300, fill = "gold", alpha = 0.5, color = "black", linewidth = 0.05) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "blue", linewidth = 0.2) +
  geom_vline(xintercept =  mu, size = 0.2, color = "black", linewidth = 0.2,  linetype = "twodash") +
  geom_vline(xintercept =  mu + 6 *sigma, color = "blue", linewidth = 0.2, linetype = "dashed") +
  geom_vline(xintercept =  mu - 6 *sigma, color = "red", linewidth = 0.2, linetype = "dashed") +
  labs(title = "Distribuzione dei Rendimenti Storici VS Curva Normale",
       subtitle = paste("S&P500 - Rendimenti giornalieri storici vs Curva Normale con Parametri pari Rendimento Medio=", percent(mu/100, accuracy = 0.01), " Deviazione STD= ", format (sigma) ),
       x = "Rendimento Giornaliero (%)",
       y = "Densità") +
  theme_minimal()

#GGPLOT LINETYPES: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash"

qqnorm(returns)
qqline(returns, col = "red")


################################################################################
# DAILY RETURNS, ARRANGED
head (df_GSPC)

# Date della settimana estrema in aprile 2025
date_da_evidenziare <- as.Date(c("2025-04-09", "2025-04-04", "2025-04-03"))
df_GSPC$Date <- as.Date(df_GSPC$Date)


# GIORNATE STRAORDINARIE! APRILE 2025 ##########################################
# Date della settimana estrema in aprile 2025
date_da_evidenziare <- as.Date(c("2025-04-09", "2025-04-04", "2025-04-03"))
df_GSPC$Date <- as.Date(df_GSPC$Date)

# Plot
df_GSPC %>%
  arrange(Daily_Returns) %>%
  mutate(
    Rank = row_number(),
    Evidenziata = Date %in% date_da_evidenziare,
    Etichetta = ifelse(Evidenziata, format(Date, "%d %b"), NA)
  ) %>%
  ggplot() +
  # Tutti gli altri punti
  geom_point(aes(x = Rank, y = Daily_Returns), color = "gray80", size = 1.5) +
  # Punti evidenziati
  geom_point(data = ~filter(., Evidenziata),
             aes(x = Rank, y = Daily_Returns), color = "blue", size = 3) +
  # Etichette
  geom_text(data = ~filter(., Evidenziata),
            aes(x = Rank, y = Daily_Returns, label = format(Date, "%d %b")),
            hjust = -0.2, vjust = 0,
            size = 3.5) +
  labs(
    title = "Estrema volatilità nello S&P 500",
    subtitle = "Nell'ultima settimana di trading il 3 e 4 aprile sono stati tra i peggiori giorni della storia,\nmentre il 9 aprile tra i migliori. Volatilità all'estremo!!",
    x = "Rank (dal peggiore al migliore rendimento giornaliero al 1928 al 09 aprile 2025)",
    y = "Rendimento giornaliero",
    caption = "Fatto con ❤️ da Alberto Frison • github.com/albertofrison/Stock_Market"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(margin = margin(b = 10), size = 12),
    plot.caption = element_text(face = "italic", size = 10, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

df_GSPC %>% 
  nrow()

df_GSPC %>%
  arrange(Daily_Returns) %>%
  mutate(
    Rank = row_number(),
    Evidenziata = Date %in% date_da_evidenziare,
    Etichetta = ifelse(Evidenziata, format(Date, "%d %b"), NA)
  ) %>% 
  filter (Date %in% date_da_evidenziare)

24434 - 24424
24434 / (365 - 52*2)

ggsave("sp500_volatility.png", width = 6, height = 6, dpi = 300)

24434 - 24424
24434 / (365 - 52*2)

ggsave("sp500_volatility.png", width = 6, height = 6, dpi = 300)

################################################################################
# RENDIMENTI MENSILI

df_GSPC$Month_Year <- format(df_GSPC$Date, "%Y-%m")
df_SP500TR$Month_Year <- format(df_SP500TR$Date, "%Y-%m")

monthly_returns <- df_SP500TR %>%
  group_by(Month_Year) %>%
  summarise(Last_Close = last(Close)) %>%
  ungroup()

monthly_returns$Monthly_Return <- c(0, (monthly_returns$Last_Close[-1] / monthly_returns$Last_Close[-nrow(monthly_returns)] -1) *100  )
returns <- monthly_returns$Monthly_Return

summary (returns)
qqnorm(returns)
qqline(returns, col = "gold")

# Calcolo della media e deviazione standard
mu <- mean(returns, na.rm = T) 
sigma <- sd(returns, na.rm = T)

# Creazione dell'istogramma con curva gaussiana
hist_chart <- ggplot(data.frame(returns), aes(x = returns)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "gold", alpha = 0.5, color = "black", linewidth = 0.05) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "blue", linewidth = 0.4) +
  geom_vline(xintercept =  mu, size = 0.2, color = "black", linewidth = 0.4,  linetype = "twodash") +
  geom_vline(xintercept =  mu + 5.5 *sigma, color = "blue", linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept =  mu - 5.5 *sigma, color = "red", linewidth = 0.4, linetype = "dashed") +
  #geom_point(aes(x = -30, y = 0.0025), color = "red", size = 15, stroke = 0.5, shape = 1) + 
  #geom_point(aes(x = 36.5, y = 0.0025), color = "red", size = 35, stroke = 0.5, shape = 1) + 
  labs(title = "Distribuzione dei Rendimenti Storici VS Curva Normale",
       subtitle = paste("S&P500 - Rendimenti mensili storici vs Curva Normale con Parametri pari Rendimento Medio=", percent(mu/100, accuracy = 0.01), " Deviazione STD= ", percent(sigma/100, accuracy = 0.01) ),
       x = "Rendimento Mensile Medio (%)",
       y = "Densità",
       caption = "Code by Alberto Frison | GitHub: github.com/albertofrison/Stock_Market") +
  theme_minimal()

hist_chart
df_returns <- data.frame(Value = returns)

df_returns <- df_returns %>%
  mutate(Theoretical = qqnorm(Value, plot.it = FALSE)$x,
         Sample = qqnorm(Value, plot.it = FALSE)$y) %>%
  arrange(Theoretical) 

bottom_left <- df_returns[1:8, ]
top_right <- df_returns[(nrow(df_returns) - 4):nrow(df_returns), ]

# Compute centers for circles
center_bottom_left <- colMeans(bottom_left)
center_top_right <- colMeans(top_right)

# Create the QQ plot
qq_chart <- ggplot(df_returns, aes(x = Theoretical, y = Sample)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +  # Trend line
  geom_point(aes(y = center_bottom_left[1], x = center_bottom_left[2]), color = "red", size = 80, stroke = 0.5, shape = 1) +  # Circle for bottom left
  geom_point(aes(y = center_top_right[1], x = center_top_right[2]), color = "red", size = 80, stroke = 0.5, shape = 1) +  # Circle for top right
  labs(title = "I rendimenti mensili dello SP500 non sono distribuiti normalmente",
       subtitle = "Quanto più i valori in blu si distanziano dalla retta nera, tanto meno la distribuzione dei rendimenti segue una normale",
       x = "Quantili Normale",
       y = "Quantili Rendimenti Mensili") +
  theme_minimal()


library(patchwork)
hist_chart
qq_chart

hist_chart + qq_chart + plot_layout(nrow = 1, ncol = 2)

#LATO OSCURO DEGLI INVESTIMENTI - PRESO DA THE BULL !###########################
# Aggiungere una colonna con l'anno
df_GSPC$Year <- format(df_GSPC$Date, "%Y")

# Prendere l'ultimo valore di Close per ogni anno
annual_close <- df_GSPC %>%
  group_by(Year) %>%
  summarise(Last_Close = last(Close)) %>%
  ungroup()

# Calcolare i return annuali
annual_close$Annual_Return <- c(NA, (annual_close$Last_Close[-1] / annual_close$Last_Close[-nrow(annual_close)] -1) *100  )

# Visualizzare i primi risultati
head(annual_close)

annual_close

target = -1

high_loss_days <- df_GSPC %>%
  filter(Daily_Returns <= target) %>%
  group_by(Year) %>%
  summarise(Count_High_Losses = n()) %>%
  ungroup()

df_binded <- left_join(annual_close, high_loss_days, by = "Year")


df_binded %>%
  filter(Year %in% c(1928:2024)) %>%
  mutate(Color_Group = case_when(
    Year == 2017 ~ "blue", # Colora il 2017 in blu
    Annual_Return > 0 ~ "darkgreen", # Verde per anni positivi
    TRUE ~ "red" # Rosso per anni negativi
  )) %>%
  ggplot(aes(x = Annual_Return, y = Count_High_Losses)) +
  
  # Punti colorati in base al rendimento annuale
  geom_point(aes(color = Color_Group), size = 3, alpha = 0.8) +
  scale_color_identity() +
  
  # Etichette migliorate con più spazio
  geom_text(aes(label = Year), vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) +
  geom_text(data = df_binded %>% filter(Year == 2017),
            aes(label = "2017"), color = "blue",
            vjust = -1.5, hjust = 0.5, size = 4, fontface = "bold") +
  
  # Linee della media più visibili
  geom_vline(aes(xintercept = mean(Annual_Return, na.rm = TRUE)), 
             linetype = "dashed", color = "blue", size = 0.5) +
  geom_hline(aes(yintercept = mean(Count_High_Losses, na.rm = TRUE)), 
             linetype = "dashed", color = "blue", size = 0.5) +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray50")
  ) +
    labs(
    title = "📈 S&P 500 - Il lato oscuro dei grandi anni [preso dalla puntata #194 di The Bull di Riccardo Spada]",
    subtitle = "Anche nei migliori anni esistono giornate che vanno male... hold on to your butts! [anni dal 1928 al 2024]",
    x = "Annual Return (%)",
    y = "Giorni con perdite giornaliere superiori al 1% (#)",
    caption = "Code by Alberto Frison | GitHub: github.com/albertofrison/Stock_Market"
  )


#SP500 E MEDIE A 50 E 200 GIORNI ###############################################
library(zoo)
library(scales)


# arrange the data frame by date
df_GSPC <- df_GSPC %>% arrange(Date)
head (df_GSPC) # check

# rollmean(x, k, ...) calcola la media mobile di 'x' su finestre di 'k' periodi.
# 'fill = NA' aggiunge NA all'inizio per avere un output della stessa lunghezza dell'input.
# 'align = "right"' assicura che la media sia calcolata usando l'osservazione corrente e le k-1 precedenti.
df_GSPC$SMA_50_Price <- rollmean(df_GSPC$Adjusted, k = 50, fill = NA, align = "right")

# 4. Calcola la media mobile a 200 periodi
df_GSPC$SMA_200_Price <- rollmean(df_GSPC$Adjusted, k = 200, fill = NA, align = "right")
head (df_GSPC) # check


data_per_plot <- df_GSPC %>%
  filter(Date > as.Date("2016-01-01")) %>%
  # Assicurati che le colonne SMA esistano e siano calcolate sul prezzo Adjusted
  # (Vedi commenti nella risposta precedente se devi ricalcolarle)
  select(Date, Adjusted, SMA_50_Price, SMA_200_Price) %>%
  pivot_longer(
    cols = c(Adjusted, SMA_50_Price, SMA_200_Price),
    names_to = "Metrica",
    values_to = "Valore"
  ) %>%
  # Definisci Metrica come factor per controllare ordine e labels nella legenda
  mutate(Metrica = factor(Metrica,
                          levels = c("Adjusted", "SMA_50_Price", "SMA_200_Price"),
                          labels = c("Prezzo Adjusted", "SMA 50 Giorni", "SMA 200 Giorni")))


data_per_plot %>% 
  ggplot() +
  geom_line (aes(x = Date, y = Valore, color= Metrica)) +
  geom_line (aes(x = Date, y = Valore, color = Metrica)) +
  geom_line (aes(x = Date, y = Valore, color = Metrica)) +
  
  # Aggiungi etichette chiare (titolo, sottotitolo, assi, legenda, fonte)
  labs(
    title = "Andamento Indice S&P 500 e Medie Mobili a 50 e 200 giorni",
    subtitle = "Prezzo giornaliero con medie mobili semplici a 50 e 200 giorni (dal 2016)",
    x = "Data",
    y = "Valore Indice",
    color = "Legenda:", # Titolo per la legenda del colore
    caption = paste("Fonte Dati: Yahoo Finance - Grafico generato il:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\nDone with ❤️ by Alberto Frison: https://github.com/albertofrison/Stock_Market/")
  ) +
  
  # Personalizza gli assi
  scale_x_date(
    date_breaks = "1 year",          # Intervalli principali ogni anno
    date_labels = "%Y",              # Mostra solo l'anno come etichetta
    date_minor_breaks = "3 months"   # Intervalli minori ogni 3 mesi (opzionale)
  ) +
  scale_y_continuous(
    labels = scales::label_number( # Formatta i numeri sull'asse Y
      accuracy = 0.01,        # Due cifre decimali (se necessario, altrimenti usa 1)
      big.mark = ".",         # Separatore migliaia italiano
      decimal.mark = ","      # Separatore decimali italiano
    ) ) +
  
  scale_color_manual(values = c(
    "Prezzo Adjusted" = "black",      # Colore per la linea Adjusted
    "SMA 50 Giorni"   = "dodgerblue", # Colore per SMA 50
    "SMA 200 Giorni"  = "firebrick"   # Colore per SMA 200
  )) +
  # Applica un tema più pulito rispetto al default
  theme_minimal(base_size = 11) + # Prova anche theme_light() o theme_bw()
  
  # Ulteriori personalizzazioni del tema
  theme(
    plot.title = element_text(face = "bold", size = rel(1)), # Titolo in grassetto e più grande
    plot.subtitle = element_text(size = rel(1), margin = margin(b = 10)), # Sottotitolo leggermente più grande
    legend.position = "bottom", # Sposta la legenda sotto il grafico
    legend.title = element_text(face = "bold"), # Titolo legenda in grassetto
    axis.title = element_text(face = "bold"), # Titoli assi in grassetto
    plot.caption = element_text(hjust = 0, face = "italic", size = rel(0.9)) # Caption a sinistra, corsivo, più piccolo
  )

ggsave("sp500_death_cross_1.png", width = 6, height = 6, dpi = 300)


# M2 MONEY #####################################################################
# LIBRERIE #####################################################################
# LIBRERIE #####################################################################
library(quantmod)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)

# G7 ##########################################################################
m2_fred_codes <- c(
  USA = "M2SL",                   # M2 USA
  FRA = "MABMM301EZM189S",        # Eurozone (Francia via Eurozona)
  DEU = "MABMM301EZM189S",        # Germania via Eurozona
  ITA = "MABMM301EZM189S",        # Italia via Eurozona
  JPN = "MABMM301JPM189S",        # Giappone
  GBR = "MABMM301GBM189S",        # Regno Unito
  CAN = "MABMM301CAM189S"         # Canada
)

fx_fred_codes <- c(
  FRA = "DEXUSEU",  # Eurozona per Francia, Italia, Germania
  DEU = "DEXUSEU",  # Eurozona
  ITA = "DEXUSEU",  # Eurozona
  JPN = "DEXJPUS",  # Giappone
  GBR = "DEXUSUK",  # Regno Unito
  CAN = "DEXCAUS",  # Canada
  USA = NA
)

# STEP 1 – SCARICA M2 #########################################################
getSymbols(m2_fred_codes, src = "FRED")
getSymbols(m2_fred_codes, src = "FRED", from = as.Date("2000-01-01"), to = Sys.Date())


m2_list <- lapply(names(m2_fred_codes), function(cc) {
  code <- m2_fred_codes[[cc]]
  xt <- get(code)
  data.frame(date = index(xt), m2_local = as.numeric(xt[, 1]), country = cc)
})

m2_data <- bind_rows(m2_list) %>%
  mutate(date = floor_date(date, "month")) %>%
  filter(!is.na(m2_local))

# STEP 2 – SCARICA FX #########################################################
getSymbols(na.omit(fx_fred_codes), src = "FRED", from = as.Date("2000-01-01"), to = Sys.Date())

fx_list <- lapply(names(fx_fred_codes), function(cc) {
  code <- fx_fred_codes[[cc]]
  if (!is.na(code) && exists(code)) {
    xt <- get(code)
    data.frame(date = index(xt), fx = as.numeric(xt[, 1]), country = cc)
  } else {
    NULL
  }
})

fx_data <- bind_rows(fx_list) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(country, date) %>%
  summarise(fx = mean(fx, na.rm = TRUE), .groups = "drop")

# STEP 3 – JOIN + CONVERSIONE #################################################
m2_fx <- m2_data %>%
  left_join(fx_data, by = c("country", "date")) %>%
  mutate(m2_usd = if_else(country == "USA", m2_local, m2_local / fx)) %>%
  filter(!is.na(m2_usd))

# STEP 4 – AGGREGAZIONE GLOBALE ###############################################
m2_aggregated <- m2_fx %>%
  group_by(date) %>%
  summarise(global_m2_usd = sum(m2_usd, na.rm = TRUE),
            countries_included = n_distinct(country)) %>%
  arrange(date)

# STEP 5 – GRAFICO ############################################################
tail(m2_aggregated)
m2_aggregated %>%
  filter (date <= "2025-02-01") %>%
  ggplot(aes(x = date, y = global_m2_usd / 1e12)) +
  geom_line(color = "darkblue", size = 1.1) +
  labs(title = "Aggregated M2 - G7 Countries (USD, Monthly)",
       subtitle = "Converted to USD using monthly exchange rates",
       x = "Date", y = "M2 in Trillions USD") +
  scale_y_continuous(labels = label_number(suffix = " T")) +
  theme_minimal()
