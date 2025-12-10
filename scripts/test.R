
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

data = read_csv2(file = paste0("C:/Users/LuizFernandoLopes/Downloads/",
                               "Series_23_10_2025_15_01_34.csv"),
                 show_col_types = FALSE) %>% 
  mutate(date  = as.Date(Data, format = "%d/%m/%Y"),
         asset = `Cota Ajustados`,
        .keep  = "none")

getSymbols(Symbols = "BAMLEMCBPITRIV", 
           src = 'FRED', 
           return.class = "data.frame")

getSymbols(Symbols = "DGS3MO", 
           src = 'FRED', 
           return.class = "data.frame")

benchmark = BAMLEMCBPITRIV %>% 
  mutate(date  = as.Date(rownames(.)),
         bench = BAMLEMCBPITRIV,
        .keep  = "none") %>% 
  tsibble::as_tibble() 

test = inner_join(x = data,
           y = benchmark) %>% 
  arrange(date) %>% 
  pivot_longer(-1) %>% 
  mutate(r = value/lag(value,252*3) - 1) %>% 
  pivot_wider(id_cols = date,
              names_from = name,
              values_from = r) %>% 
  drop_na() %>% 
  mutate(x_r = (1 + asset)/(1 + bench) - 1,
         pos = ifelse(x_r > 0, 1, 0),
         n_excess = cumsum(pos),
         `%_excess` = (cumsum(pos)/length(unique(.$date))) * 100,
         cum_rolling756 = zoo::rollapplyr(pos, width = 504, FUN = sum, partial = TRUE),
         `%_rolling756` =  (cum_rolling756 / 504) * 100) %>% 
  #Consistência de Retorno
  mutate(median = median(x_r, na.rm = TRUE),
         skew = skewness(x_r, na.rm = TRUE),
         kurtosis = kurtosis(x_r, na.rm = TRUE)) %>% 
  #Probabilidade de Retorno
  mutate(cdf = 1 - ecdf(x_r)(0))


test %>% 
  ggplot() +
  aes(x = date, y = x_r) +
  geom_line()

test %>% 
  ggplot() +
  geom_line(aes(date, `%_excess`)) +
  geom_hline(yintercept = median(data$`%_excess`),
             color = "red", 
             linewidth = .75, 
             linetype = "dashed") +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank())

test %>% 
  ggplot() +
  geom_line(aes(date, `%_rolling756`)) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank())

test %>% 
  ggplot(aes(x = x_r)) +
  geom_histogram(aes(y = after_stat(density)), fill = "grey" , bins = 50) +
  geom_density() +
  geom_vline(xintercept = 0) +
  geom_vline(data = test, aes(xintercept = median), linetype = "dashed") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title = element_blank(), 
        strip.background = element_blank()) + 
  geom_text(aes(x = -Inf, 
                y = Inf, 
                hjust = 0,
                vjust = 1,
                label = paste(" skew: ", 
                              round(skew,2),
                              "\n",
                              "kurtosis: ",
                              round(kurtosis,2))),
            colour = "red", size = 3.75)

test |>
  ggplot(aes(x = x_r)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  stat_ecdf(geom = "step") +
  geom_text(aes(x = -Inf, y = Inf, hjust = 0, vjust = 1, label = paste(round(cdf,2))),
            colour = "red", size = 4) + 
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom", 
                     legend.title = element_blank(), 
                     axis.title = element_blank(), 
                     strip.background = element_blank())

