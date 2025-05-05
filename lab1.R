library(tidyverse)

erp_load <- function(path) {
  filenames <- list.files(path = path, full.names = TRUE)  
  lista_dati <- vector("list", length(filenames))  
  
  for (i in seq_along(filenames)) {
    tempdata <- read.table(filenames[i], header = TRUE) 
    
    temp_long <- tempdata %>%
      pivot_longer(cols = -time, names_to = "channel", values_to = "value") %>%
      mutate(
        id = ceiling(i / 4),  
        condition = c("Cars", "Faces", "ScrambledCars", "ScrambledFaces")[(i - 1) %% 4 + 1]  
      )
    
    lista_dati[[i]] <- temp_long  # Salva il tibble trasformato
    }
  
  df_finale <- bind_rows(lista_dati)  # Combina tutti i dati
  return(df_finale)
  rm(lista_dati)
}

df_finale <- erp_load("C:/Users/david/Downloads/ERPDataforstudents")


plot_mean_values  <- function(data, channels_selected, conditions_selected) {
  # Filtra i dati per i canali e le condizioni selezionate
  data_filtered <- data %>%
    filter(channel %in% channels_selected, condition %in% conditions_selected) %>%
    group_by(time, channel, condition) %>%  # Aggiunto 'channel' per gestire più grafici
    summarise(
      mean_value = mean(value, na.rm = TRUE),  # Media dei valori
      sem_value = sd(value, na.rm = TRUE) / sqrt(n())  # Errore standard della media
    )  
  
  # Controlla se ci sono dati validi
  if (nrow(data_filtered) == 0) {
    message("Nessun dato trovato per i canali: ", paste(channels_selected, collapse = ", "), 
            " e le condizioni: ", paste(conditions_selected, collapse = ", "))
    return(NULL)
    }
  
  # Crea il grafico con ggplot2 e facet_wrap per più canali
  ggplot(data_filtered, aes(x = time, y = mean_value, color = condition, fill = condition)) +
    geom_line(linewidth = 1) +  # Linea per la media
    geom_ribbon(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value), alpha = 0.2) +  # Banda errore standard
    facet_wrap(~ channel, scales = "free_y") +  # Genera più grafici, uno per canale
    labs(
      x = "Tempo",
      y = "Valore Medio",
      color = "Condizione",
      fill = "Condizione"
    ) +
    theme_minimal() +  # Tema pulito
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Titolo centrato
      legend.position = "top"
    )
}


plot_mean_values(df_finale, c("F3","F4","PO7","PO8"),c("Faces","ScrambledFaces"))

library(tidyverse)

library(tidyverse)  # Assicura che dplyr sia caricato correttamente

plot_multiple_conditions <- function(data, channel_selected, conditions_selected) {
  # Filtra i dati per il canale e le condizioni selezionate
  data_filtered <- data %>%
    filter(channel == channel_selected, condition %in% conditions_selected)
  
  # Calcola la media per ogni condizione
  data_summary <- data_filtered %>%
    group_by(time, condition) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )  
  
  # Controlla se ci sono dati validi
  if (nrow(data_summary) == 0) {
    message("Nessun dato trovato per il canale: ", channel_selected, 
            " e le condizioni: ", paste(conditions_selected, collapse = ", "))
    return(NULL)
  }
  
  # Crea il grafico
  ggplot() +
    # Linee nere per i singoli soggetti
    geom_line(data = data_filtered, aes(x = time, y = value, group = id), 
              color = "black", linewidth = 0.5) +  
    # Linea rossa per la media
    geom_line(data = data_summary, aes(x = time, y = mean_value), 
              color = "red", linewidth = 1.2) +  
    # Facet per mostrare più condizioni in grafici separati
    facet_wrap(~ condition, scales = "free_y") + 
    labs(
      title = paste("Canale:", channel_selected, "- Confronto Condizioni"),
      x = "Tempo",
      y = "Valore",
      color = "Condizione"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}




plot_multiple_conditions(df_finale, channel_selected = c("FP1"), conditions_selected = c("Cars", "Faces", "ScrambledCars", "ScrambledFaces"))

plot_mean_rect  <- function(data, channels_selected, conditions_selected) {
  # Filtra i dati per i canali e le condizioni selezionate
  data_filtered <- data %>%
    filter(channel %in% channels_selected, condition %in% conditions_selected) %>%
    group_by(time, channel, condition) %>%  # Aggiunto 'channel' per gestire più grafici
    summarise(
      mean_value = mean(value, na.rm = TRUE),  # Media dei valori
      sem_value = sd(value, na.rm = TRUE) / sqrt(n())  # Errore standard della media
    )  
  
  # Controlla se ci sono dati validi
  if (nrow(data_filtered) == 0) {
    message("Nessun dato trovato per i canali: ", paste(channels_selected, collapse = ", "), 
            " e le condizioni: ", paste(conditions_selected, collapse = ", "))
    return(NULL)
  }
  
  # Crea il grafico con ggplot2 e facet_wrap per più canali
  ggplot(data_filtered, aes(x = time, y = mean_value, color = condition, fill = condition)) +
    geom_rect(aes(xmin = 130, xmax = 180, ymin = -Inf, ymax = Inf), 
              fill = "lightgrey", alpha = 0.1,color = "lightgrey", linewidth = 0.05) +
    geom_line(linewidth = 1) +  # Linea per la media
    geom_ribbon(aes(ymin = mean_value - sem_value, ymax = mean_value + sem_value), alpha = 0.2) +  # Banda errore standard
    facet_wrap(~ channel, scales = "free_y") +  # Genera più grafici, uno per canale
    labs(
      x = "Tempo",
      y = "Valore Medio",
      color = "Condizione",
      fill = "Condizione"
    ) +
    theme_bw() +  # Tema pulito
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Titolo centrato
      legend.position = "top"
    )
}

plot_mean_rect(df_finale, c("P7","P8"),c("Faces","ScrambledFaces"))





