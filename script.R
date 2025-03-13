################################################################################
# STOCK MARKET BY ALBERTO FRISON
# R PROJECT TO ANALYSE HISTORICAL MARKET DATA
# DONE WITH ♥ BY ALBERTO FRISON
################################################################################


################################################################################
# LIBRARIES & INITIALIZATION
library (tidyverse)
library(quantmod)

rm (list = ls()) # cleans up objects in the environment


################################################################################

getSymbols(Symbols = c("^GSPC"), src = "yahoo", from = "1900-01-01", to = Sys.Date())
head (GSPC)

# CONVERTING INTO A DATAFRAME
df_GSPC <- data.frame (Date = index(GSPC), coredata(GSPC))
colnames(df_GSPC) <- c ("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# SELECTING THE FIRST ENTRY OF THE HISTORICAL DATA TO EVENTUALLY PERFORM AN INDEXING CALCULATION
first_Date <- min(df_GSPC$Date)
first_Close <- df_GSPC$Close [df_GSPC$Date == first_Date]


df_GSPC <- df_GSPC %>%
  mutate (Close_Index = Close / first_Close * 100) %>%
  select (Date, Close, Close_Index)

df_GSPC$Daily_Returns <- c(NA, (df_GSPC$Close[-1] / df_GSPC$Close[-nrow(df_GSPC)] - 1)*100)


################################################################################
# RENDIMENTI GIORNALIERI
returns <- df_GSPC$Daily_Returns

# Calcolo della media e deviazione standard
mu <- mean(returns, na.rm = T)
sigma <- sd(returns, na.rm = T)

# Creazione dell'istogramma con curva gaussiana
ggplot(data.frame(returns), aes(x = returns)) +
  geom_histogram(aes(y = ..density..), bins = 300, fill = "gold", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "blue", size = 0.5) +
  geom_vline(xintercept =  mu) +
  geom_vline(xintercept =  mu + 6 *sigma) +
  geom_vline(xintercept =  mu - 6 *sigma) +
  labs(title = "Istogramma dei Rendimenti con Curva Normale",
       x = "Rendimento",
       y = "Densità") +
  theme_minimal()


qqnorm(returns)
qqline(returns, col = 2)


################################################################################
# RENDIMENTI MENSILI
df_GSPC$Month_Year <- format(df_GSPC$Date, "%Y-%m")

monthly_returns <- df_GSPC %>%
  group_by(Month_Year) %>%
  summarise(Last_Close = last(Close)) %>%
  ungroup()


monthly_returns$Monthly_Return <- c(NA, (monthly_returns$Last_Close[-1] / monthly_returns$Last_Close[-nrow(monthly_returns)] -1) *100  )

summary (monthly_returns)
mean(monthly_returns$Monthly_Return, na.rm = T)
sd(monthly_returns$Monthly_Return, na.rm = T)
hist(monthly_returns$Monthly_Return, breaks = 100, freq = F)
curve(dnorm(x, mean= mean(monthly_returns$Monthly_Return), sd= sd (monthly_returns$Monthly_Return)), lwd=2, add=TRUE)
?curve
?dnorm
qqnorm(monthly_returns$Monthly_Return)
qqline(monthly_returns$Monthly_Return, col = 2)


# Generazione di un esempio di rendimenti mensili
returns <- monthly_returns$Monthly_Return

# Calcolo della media e deviazione standard
mu <- mean(returns, na.rm = T)
sigma <- sd(returns, na.rm = T)

# Creazione dell'istogramma con curva gaussiana
ggplot(data.frame(returns), aes(x = returns)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "gold", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma), color = "blue", size = 1.2) +
  labs(title = "Istogramma dei Rendimenti con Curva Normale",
       x = "Rendimento",
       y = "Densità") +
  theme_minimal()



################################################################################
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
