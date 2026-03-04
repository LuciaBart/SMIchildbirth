###multíparas 36M
library(ggplot2)
library(googlesheets4)
library(caret)    
library(groupdata2)
library(pROC)
library(dplyr)
library(rsample)
library(do)
library(glue)
library(stringr)
library(haven)
library(stringr)
library(survey)
library(writexl)
library(patchwork)

#Load data from 36M, women with more than 2 childs, last 2
load("Analysis/Question_3/Paper_sub/multi.RData")

##################################
###institucional a domiciliario###
##################################

id <- multi %>% 
  dplyr::filter(categoria == "i_d") 

#índice
institucional_36 <- id %>%
  dplyr::filter(!is.na(index_d_qoc)) %>% 
  dplyr::filter(tipoparto == "institucional") %>% 
  dplyr::select(PAIS, WEIGHT_WOMAN, WTSEG, index_d_qoc)

table(institucional_36$index_d_qoc, institucional_36$PAIS)

calcular_proporciones <- function(institucional_36) {
  # Obtener los países únicos
  paises <- unique(institucional_36$PAIS)
  
  # Obtener los valores únicos de index_d_qoc
  indices <- sort(unique(institucional_36$index_d_qoc))
  
  # Crear un data.frame para almacenar los resultados
  resultados <- data.frame(PAIS = paises)
  
  # Para cada país, calcular las proporciones para cada valor de index_d_qoc
  for (pais in paises) {
    # Filtrar datos para este país
    datos_pais <- subset(institucional_36, PAIS == pais)
    
    # Calcular el peso total para este país
    peso_total <- sum(datos_pais$WEIGHT_WOMAN)
    
    # Para cada valor de index_d_qoc, calcular la proporción
    for (indice in indices) {
      # Filtrar para este índice
      datos_indice <- subset(datos_pais, index_d_qoc == indice)
      
      # Si hay datos para este índice, calcular la proporción
      if (nrow(datos_indice) > 0) {
        peso_indice <- sum(datos_indice$WEIGHT_WOMAN)
        proporcion <- peso_indice / peso_total
      } else {
        proporcion <- 0  # Si no hay datos para este índice
      }
      
      # Agregar al data.frame de resultados
      resultados[resultados$PAIS == pais, paste0("index_", indice)] <- proporcion
    }
  }
  
  return(resultados)
}

# Ejecutar la función
proporciones <- calcular_proporciones(institucional_36)

# Mostrar los resultados
print(proporciones)

library(writexl)
write_xlsx(proporciones, "id_multi_index_prop.xlsx")

df_long <- proporciones %>%
  pivot_longer(cols = starts_with("index"), names_to = "index", values_to = "valor")

df_long$index <- factor(df_long$index)

df_long$index <- factor(df_long$index, levels = rev(levels(df_long$index)))

conteo <- institucional_36 %>% 
  group_by(PAIS) %>%
  summarise(N_total = n())

conteo <- conteo %>%
  mutate(PAIS = case_when(
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    PAIS == "HND" ~ "Honduras",
    PAIS == "GTM" ~ "Guatemala",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

df_long <- df_long %>%
  mutate(PAIS = case_when(
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    PAIS == "HND" ~ "Honduras",
    PAIS == "GTM" ~ "Guatemala",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

df_long <- df_long %>%
  mutate(index = case_when(
    index == "index_0" ~ "0/4",
    index == "index_1" ~ "1/4",
    index == "index_2" ~ "2/4",
    index == "index_3" ~ "3/4",
    index == "index_4" ~ "4/4",
    TRUE ~ index # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

final_grafico <- df_long %>%
  left_join(conteo, by = c("PAIS"))

aux_data <- final_grafico %>%
  group_by(PAIS) %>%
  slice(1) %>%  # Toma solo la primera fila de cada grupo
  ungroup()

nombres_paises <- c("4 (4/4 affirmative domains)", "3 (3/4 affirmative domains)", "2 (2/4 affirmative domains)", "1 (1/4 affirmative domains)", "0 (0/4 affirmative domains)") 

index_plot_id <- ggplot(df_long, aes(x = index, y = valor)) +
  geom_col(position = "dodge", fill = "grey") +
#  geom_text(data = aux_data, aes(x = index, y = 1, label = paste("N=", N_total, sep="")),
#            position = position_dodge(width = 0.9), vjust = -0.5, size =4) +
#  geom_text(aes(y = 0.1, label = index, group = factor(index)),
#            position = position_dodge(width = 0.9), vjust = 0.5, size = 3, color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "First child",
       subtitle = "Score proportion distribution",
       # subtitle = "Quality domains",
       x = "Index", y = "Proportion of live births with each score") +
  facet_wrap(~ PAIS, scales = "free_y", ncol = 1) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = element_text(face = "bold")) +
  coord_flip()

print(index_plot_id)


#razones

domiciliario <- id %>%
  dplyr::filter(tipoparto == "domiciliario") %>% 
  dplyr::select(TX_AREA, PAIS, WEIGHT_WOMAN, WTSEG, starts_with("R_"))

domiciliario_transformado <- domiciliario %>%
  dplyr::mutate(
    R_Knowledge = ifelse(R_Knowledge == 1, 1, 0),
    R_Accesibility = ifelse(R_Accesibility == 1, 2, 0),
    R_Infrastructure = ifelse(R_Infrastructure == 1, 3, 0),
    R_treatmentandcommunication = ifelse(R_treatmentandcommunication == 1, 4, 0),
    R_Family = ifelse(R_Family == 1, 5, 0),
    R_Cultural_Preferences = ifelse(R_Cultural_Preferences == 1, 6, 0)
  )


domiciliario_apilado <- domiciliario %>%
  pivot_longer(
    cols = starts_with("R_"),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  dplyr::mutate(
    Valor = case_when(
      Variable == "R_Knowledge" & Valor == 1 ~ 1,
      Variable == "R_Accesibility" & Valor == 1 ~ 2,
      Variable == "R_Infrastructure" & Valor == 1 ~ 3,
      Variable == "R_treatmentandcommunication" & Valor == 1 ~ 4,
      Variable == "R_Family" & Valor == 1 ~ 5,
      Variable == "R_Cultural_Preferences" & Valor == 1 ~ 6,
      TRUE ~ 0
    )
  ) 

domiciliario_apilado <- domiciliario_apilado %>% 
  dplyr::filter(Valor != "0")

diseno <- svydesign(
  id = ~WTSEG,         
  weights = ~WEIGHT_WOMAN,  
  data = domiciliario_apilado
)

tabla_contingencia <- svytable(~Valor + PAIS, design = diseno)

# Calcular proporciones por PAIS
props <- prop.table(tabla_contingencia, margin = 2) #* 100

print(props)

props_df <- as.data.frame.table(props)

names(props_df) <- c("Valor", "PAIS", "Proporcion")

nombres_dominios <- c("Knowledge domain", "Accesibility domain", "Infrastructure domain", "Treatment and Communication domain", "Family domain", "Cultural Preferences domain")


#uso dataset conteo para el N porque es igual

props_df <- props_df %>%
  mutate(PAIS = case_when(
    PAIS == "GTM" ~ "Guatemala",
    PAIS == "HND" ~ "Honduras",
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

write_xlsx(props_df, "id_multi_reasons_prop.xlsx")

reasons_plot_id <- props_df %>%
  mutate(
    PAIS = factor(PAIS, levels = c("Chiapas", "Guatemala", "Honduras", "Nicaragua")),
    dominio_razon = case_when(
    Valor == "1" ~ "Knowledge",
    Valor == "2" ~ "Accesibility",
    Valor == "3" ~ "Infrastructure",
    Valor == "4" ~ "Treatment and Communication",
    Valor == "5" ~ "Family",
    Valor == "6" ~ "Cultural Preferences",
    TRUE ~ Valor
  )) %>%
ggplot(aes(x = dominio_razon, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "grey") +
  facet_wrap(~ PAIS, ncol = 1, labeller = labeller(PAIS = function(x) 
    c("Chiapas", "Guatemala", "Honduras", "Nicaragua"))) +
  labs(
    title = "Second child",
    subtitle = "Proportion of reason domains",
    x = "Reason domain",
    y = "Proportion of reason choice"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  coord_flip()

print(reasons_plot_id)

combined_plot_id <- index_plot_id + reasons_plot_id + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "Switch from institutional to home delivery",
    subtitle = "Chiapas N = 25 | Guatemala N = 50 | Honduras N = 8 | Nicaragua N =12",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5)) # Añadimos hjust = 0.5 al subtítulo
  )

print(combined_plot_id)

##################################
###domiciliario a institucional### 
##################################

di <- multi %>% 
  dplyr::filter(categoria == "d_i")

#índice
institucional_36 <- di %>%
  dplyr::filter(!is.na(index_d_qoc)) %>% 
  dplyr::filter(tipoparto == "institucional") %>% 
  dplyr::select(PAIS, WEIGHT_WOMAN, WTSEG, index_d_qoc)

table(institucional_36$index_d_qoc, institucional_36$PAIS)

calcular_proporciones <- function(institucional_36) {
  # Obtener los países únicos
  paises <- unique(institucional_36$PAIS)
  
  # Obtener los valores únicos de index_d_qoc
  indices <- sort(unique(institucional_36$index_d_qoc))
  
  # Crear un data.frame para almacenar los resultados
  resultados <- data.frame(PAIS = paises)
  
  # Para cada país, calcular las proporciones para cada valor de index_d_qoc
  for (pais in paises) {
    # Filtrar datos para este país
    datos_pais <- subset(institucional_36, PAIS == pais)
    
    # Calcular el peso total para este país
    peso_total <- sum(datos_pais$WEIGHT_WOMAN)
    
    # Para cada valor de index_d_qoc, calcular la proporción
    for (indice in indices) {
      # Filtrar para este índice
      datos_indice <- subset(datos_pais, index_d_qoc == indice)
      
      # Si hay datos para este índice, calcular la proporción
      if (nrow(datos_indice) > 0) {
        peso_indice <- sum(datos_indice$WEIGHT_WOMAN)
        proporcion <- peso_indice / peso_total
      } else {
        proporcion <- 0  # Si no hay datos para este índice
      }
      
      # Agregar al data.frame de resultados
      resultados[resultados$PAIS == pais, paste0("index_", indice)] <- proporcion
    }
  }
  
  return(resultados)
}

# Ejecutar la función
proporciones <- calcular_proporciones(institucional_36)

# Mostrar los resultados
print(proporciones)

write_xlsx(proporciones, "di_multi_index_prop.xlsx")


df_long <- proporciones %>%
  pivot_longer(cols = starts_with("index"), names_to = "index", values_to = "valor")

df_long$index <- factor(df_long$index)

df_long$index <- factor(df_long$index, levels = rev(levels(df_long$index)))

conteo <- institucional_36 %>% 
  group_by(PAIS) %>%
  summarise(N_total = n())

conteo <- conteo %>%
  mutate(PAIS = case_when(
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    PAIS == "HND" ~ "Honduras",
    PAIS == "GTM" ~ "Guatemala",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

df_long <- df_long %>%
  mutate(PAIS = case_when(
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    PAIS == "HND" ~ "Honduras",
    PAIS == "GTM" ~ "Guatemala",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

df_long <- df_long %>%
  mutate(index = case_when(
    index == "index_0" ~ "0/4",
    index == "index_1" ~ "1/4",
    index == "index_2" ~ "2/4",
    index == "index_3" ~ "3/4",
    index == "index_4" ~ "4/4",
    TRUE ~ index # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

final_grafico <- df_long %>%
  left_join(conteo, by = c("PAIS"))

aux_data <- final_grafico %>%
  group_by(PAIS) %>%
  slice(1) %>%  # Toma solo la primera fila de cada grupo
  ungroup()

index_plot_di <- ggplot(df_long, aes(x = index, y = valor)) +
  geom_col(position = "dodge", fill = "grey") +
  #  geom_text(data = aux_data, aes(x = index, y = 1, label = paste("N=", N_total, sep="")),
  #            position = position_dodge(width = 0.9), vjust = -0.5, size =4) +
  #  geom_text(aes(y = 0.1, label = index, group = factor(index)),
  #            position = position_dodge(width = 0.9), vjust = 0.5, size = 3, color = "black") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Second child",
       subtitle = "Score proportion distribution",
       # subtitle = "Quality domains",
       x = "Index", y = "Proportion of live births with each score") +
  facet_wrap(~ PAIS, scales = "free_y", ncol = 1) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = element_text(face = "bold")) +
  coord_flip()

print(index_plot_di)

#razones

domiciliario <- di %>%
  dplyr::filter(tipoparto == "domiciliario") %>% 
  dplyr::select(TX_AREA, PAIS, WEIGHT_WOMAN, WTSEG, starts_with("R_"))

domiciliario_transformado <- domiciliario %>%
  dplyr::mutate(
    R_Knowledge = ifelse(R_Knowledge == 1, 1, 0),
    R_Accesibility = ifelse(R_Accesibility == 1, 2, 0),
    R_Infrastructure = ifelse(R_Infrastructure == 1, 3, 0),
    R_treatmentandcommunication = ifelse(R_treatmentandcommunication == 1, 4, 0),
    R_Family = ifelse(R_Family == 1, 5, 0),
    R_Cultural_Preferences = ifelse(R_Cultural_Preferences == 1, 6, 0)
  )


domiciliario_apilado <- domiciliario %>%
  pivot_longer(
    cols = starts_with("R_"),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  dplyr::mutate(
    Valor = case_when(
      Variable == "R_Knowledge" & Valor == 1 ~ 1,
      Variable == "R_Accesibility" & Valor == 1 ~ 2,
      Variable == "R_Infrastructure" & Valor == 1 ~ 3,
      Variable == "R_treatmentandcommunication" & Valor == 1 ~ 4,
      Variable == "R_Family" & Valor == 1 ~ 5,
      Variable == "R_Cultural_Preferences" & Valor == 1 ~ 6,
      TRUE ~ 0
    )
  ) %>%
  dplyr::select(WTSEG, WEIGHT_WOMAN, Valor, TX_AREA, PAIS)

domiciliario_apilado <- domiciliario_apilado %>% 
  dplyr::filter(Valor != "0")

diseno <- svydesign(
  id = ~WTSEG,         
  weights = ~WEIGHT_WOMAN,  
  data = domiciliario_apilado
)

tabla_contingencia <- svytable(~Valor + PAIS, design = diseno)

# Calcular proporciones por PAIS
props <- prop.table(tabla_contingencia, margin = 2) #* 100

print(props)

props_df <- as.data.frame.table(props)

names(props_df) <- c("Valor", "PAIS", "Proporcion")

nombres_dominios <- c("Knowledge domain", "Accesibility domain", "Infrastructure domain", "Treatment and Communication domain", "Family domain", "Cultural Preferences domain")


#uso dataset conteo para el N porque es igual

props_df <- props_df %>%
  mutate(PAIS = case_when(
    PAIS == "GTM" ~ "Guatemala",
    PAIS == "HND" ~ "Honduras",
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    TRUE ~ PAIS # Mantiene los valores existentes si no coinciden con los casos anteriores
  ))

write_xlsx(props_df, "di_multi_reasons_prop.xlsx")

reasons_plot_di <- props_df %>%
  mutate(
    PAIS = factor(PAIS, levels = c("Chiapas", "Guatemala", "Honduras", "Nicaragua")),
    dominio_razon = case_when(
      Valor == "1" ~ "Knowledge",
      Valor == "2" ~ "Accesibility",
      Valor == "3" ~ "Infrastructure",
      Valor == "4" ~ "Treatment and Communication",
      Valor == "5" ~ "Family",
      Valor == "6" ~ "Cultural Preferences",
      TRUE ~ Valor
    )) %>%
  ggplot(aes(x = dominio_razon, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "grey") +
  facet_wrap(~ PAIS, ncol = 1, labeller = labeller(PAIS = function(x) 
    c("Chiapas", "Guatemala", "Honduras", "Nicaragua"))) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "First child",
    subtitle = "Proportion of reason domains",
    x = "Reason domain",
    y = "Proportion of reason choice"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")) +
  coord_flip()

print(reasons_plot_di)

combined_plot_di <- reasons_plot_di + index_plot_di + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(
    title = "Switch from home to institutional delivery",
    subtitle = "Chiapas N = 39 | Guatemala N = 48 | Honduras N = 38 | Nicaragua N =17",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5)) # Añadimos hjust = 0.5 al subtítulo
  )


print(combined_plot_di)

svg("combined_plot_di.svg")
svg("combined_plot_id.svg")
ggsave(filename = "combined_plot_di.svg", plot = combined_plot_di, width = 10, height = 8)
ggsave(filename = "combined_plot_id.svg", plot = combined_plot_id, width = 10, height = 8)
