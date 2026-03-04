#####Birth setting distribution analysis

library(ggplot2)
library(forcats)
library(dplyr)
library(readxl)
library(survey)
library(tidyr)
library(ggplot2)
library(googlesheets4)
library(caret)    
library(groupdata2)
library(pROC)
library(rsample)
library(do)
library(glue)
library(stringr)
library(haven)
library(stringr)
library(survey)
library(writexl)

#Load dataset from 3 FUP, all used variables
load("Data/dataset_unido.RData")

#Distribution of birth settings (home or institutional) presented as weight percentages
prop <- dataset_unido %>%
  dplyr::select(PAIS, WEIGHT_WOMAN, WTSEG, tipoparto, FUP, TX_AREA)

diseno_encuesta <- svydesign(ids = ~WTSEG, weights = ~WEIGHT_WOMAN, data = prop) 

resultados <- svyby(~factor(tipoparto), ~PAIS + FUP, diseno_encuesta, svymean)

resultados_proporciones <- resultados %>%
  dplyr::select(PAIS, FUP, `factor(tipoparto)domiciliario`, `factor(tipoparto)institucional`) %>%
  rename(
    `domiciliario` = `factor(tipoparto)domiciliario`,
    `institucional` = `factor(tipoparto)institucional`
  ) %>%
  pivot_longer(cols = c(domiciliario, institucional), names_to = "tipoparto", values_to = "proporcion")

resultados_proporciones <- resultados_proporciones %>%
  mutate(FUP = factor(FUP, levels = c("BASELINE", "36M", "54M")))

#Table for graph
grafico <- resultados_proporciones %>% 
  pivot_wider(names_from = tipoparto, values_from = proporcion) %>%
  rename(
    proporcion_domiciliario = domiciliario,
    proporcion_institucional = institucional
  )

conteo <- dataset_unido %>% 
  group_by(PAIS, FUP) %>%
  summarise(N_total = n())

final_grafico <- grafico %>%
  left_join(conteo, by = c("PAIS", "FUP"))

# Function for comparing proportions between FUPs
comparar_proporciones <- function(pais, fup1, fup2, datos, diseno) {
  diseno_subconjunto <- subset(diseno, PAIS == pais & (FUP == fup1 | FUP == fup2))
  tabla <- svytable(~tipoparto + FUP, diseno_subconjunto)
  if (all(dim(tabla) == c(2, 2))) {
    prueba_chi_cuadrado <- svychisq(~tipoparto + FUP, diseno_subconjunto)
    return(prueba_chi_cuadrado$p.value)
  } else {
    return(NA)
  }
}

# Apply function for comparing countries and FUP (without duplicates)
resultados_comparaciones <- resultados_proporciones %>%
  group_by(PAIS) %>%
  summarise(
    p_baseline_36m = if (all(c("BASELINE", "36M") %in% FUP)) comparar_proporciones(PAIS[1], "BASELINE", "36M", cur_data(), diseno_encuesta) else NA,
    p_36m_54m = if (all(c("36M", "54M") %in% FUP)) comparar_proporciones(PAIS[1], "36M", "54M", cur_data(), diseno_encuesta) else NA,
    p_baseline_54m = if (all(c("BASELINE", "54M") %in% FUP)) comparar_proporciones(PAIS[1], "BASELINE", "54M", cur_data(), diseno_encuesta) else NA
  )

# Join comparison with proportion
resultados_finales <- resultados_proporciones %>%
  left_join(resultados_comparaciones, by = "PAIS") %>%
  distinct() # Eliminar filas duplicadas


#Arrange names for figure
final_grafico <- final_grafico %>%
  mutate(
    PAIS = case_when(
      PAIS == "GTM" ~ "Guatemala",
      PAIS == "HND" ~ "Honduras",
      PAIS == "MEX" ~ "Chiapas",
      PAIS == "NIC" ~ "Nicaragua",
      TRUE ~ PAIS 
    )
  )

final_grafico <- final_grafico %>% 
  mutate(
    FUP = case_when(
      FUP == "BASELINE" ~ "Baseline",
      FUP == "36M" ~ "Second FUP",
      FUP == "54M" ~ "Third FUP",
      TRUE ~ FUP ))

final_grafico <- final_grafico %>%
  rename(Country = PAIS)

# Reorder Country
final_grafico$Country <- factor(final_grafico$Country, levels = c("Honduras", "Nicaragua", "Chiapas", "Guatemala"))

# Reorder FUP for chronological order (BASELINE, 36M, 54M)
final_grafico$FUP <- factor(final_grafico$FUP, levels = c("Baseline", "Second FUP", "Third FUP"))

# Create label
final_grafico <- final_grafico %>%
  mutate(label = paste0(Country, " - ", FUP, " (N = ", N_total, ")"))

# Long format for ggplot
datos_long <- final_grafico %>%
  dplyr::select(Country, FUP, label, proporcion_domiciliario, proporcion_institucional, N_total) %>%
  pivot_longer(cols = c(proporcion_domiciliario, proporcion_institucional),
               names_to = "Delivery_Type",
               values_to = "Percentage")

# Create combined variable (country and FUP) for Y axis
datos_long <- datos_long %>%
  mutate(Country_FUP = paste0(Country, " - ", FUP))

datos_long <- datos_long %>%
  mutate(Country_FUP_N = paste0(Country, " - ", FUP, " - ", N_total))

# Reorder levels
datos_long$Country_FUP <- factor(datos_long$Country_FUP, 
                                 levels = rev(unique(datos_long$Country_FUP)))

# Rename delivery for label
datos_long$Delivery_Type <- ifelse(datos_long$Delivery_Type == "proporcion_institucional", 
                                   "Institutional delivery", "Home birth")

# Reorder factors
datos_long$Country <- factor(datos_long$Country, 
                             levels = c("Honduras", "Nicaragua", "Chiapas", "Guatemala"))
datos_long$FUP <- factor(datos_long$FUP, levels = c("Baseline", "Second FUP", "Third FUP"))

datos_long$Country_FUP <- factor(datos_long$Country_FUP, 
                                 levels = rev(unique(datos_long$Country_FUP)))

# Change to percentage
datos_long$Percentage_100 <- datos_long$Percentage * 100

# Create dataframe for labels with total N
total_births <- datos_long %>%
  dplyr::select(Country_FUP, N_total) %>%
  distinct()

# Create figure
graf <-ggplot(datos_long, aes(x = Percentage_100, y = Country_FUP, fill = Delivery_Type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage_100), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("Home birth" = "#5d8060", 
                               "Institutional delivery" = "#32a887")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), 
                     limits = c(0, 100),
                     breaks = seq(0, 100, 25)) +
  geom_text(data = total_births, 
            aes(x = 101, y = Country_FUP, 
                label = paste0("N = ", Total_births_N)),
            hjust = 0, size = 3.5, inherit.aes = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.margin = margin(r = 80, l = 10, t = 10, b = 10)
  ) +
  coord_cartesian(clip = "off", xlim = c(0, 120))

total_births <- datos_long %>%
  group_by(Country_FUP) %>%
  summarise(N_total = first(N_total))

datos_long <- datos_long %>%
  dplyr::mutate(Percentage_NR = round(Percentage_100, 2))

datos_long <- datos_long %>%
  dplyr::mutate(Percentage_100 = round(Percentage_100, 0))

df_long_updated <- datos_long %>%
  mutate(
    FUP = case_when(
      Country == "Honduras" & FUP == "Second FUP" ~ "Second FUP*",
      Country == "Honduras" & FUP == "Third FUP" ~ "Third FUP*",
      TRUE ~ as.character(FUP)
    )
  )

graf <- ggplot(df_long_updated, aes(y = FUP, x = Percentage_100, fill = Delivery_Type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(aes(label = paste0(round(Percentage_NR), "%")),
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            color = "white", size = 3) +
  guides(fill = guide_legend(reverse = TRUE, title = "Delivery type")) +
  scale_fill_manual(values = c("Home birth" = "#D71B60",
                               "Institutional delivery" = "#46B487")) +
  scale_y_discrete() +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  geom_text(aes(x = 105, label = paste0("N = ", N_total)),
            hjust = 0, vjust = 0.5, size = 3.5) +
  facet_wrap(~ Country, scales = "free_y", ncol = 2) +  
  theme_minimal() +
  labs(title = "Birth settings distribution") +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(angle = 0, hjust = 1),
    plot.margin = margin(r = 80, l = 80, t = 10, b = 10),
    strip.text = element_text(face = "bold")
  )
print(graf)

#ggsave("fig2prueba.svg", plot = graf, width = 10, height = 8) 

#####Figures 3 and 5
#Load dataset from 3 FUP, all used variables
load("Data/dataset_unido.RData")

dataset_unido <- dataset_unido %>%
  mutate(index_d_qoc = rowSums(dplyr::select(., starts_with("Q_")), na.rm = TRUE))

institucional <- dataset_unido %>%
  dplyr::filter(tipoparto == "institucional")

# Ponderated proportion for institutional delivery

institucional_36 <- institucional %>%
  dplyr::select(TX_AREA, PAIS, WEIGHT_WOMAN, WTSEG, index_d_qoc, FUP)

institucional_36 <- institucional_36 %>%
  mutate(FUP = case_when(
    FUP == "BASELINE" ~ "Baseline",
    FUP == "36M" ~ "Second FUP",
    FUP == "54M" ~ "Third FUP",
    TRUE ~ FUP 
  ))


diseno_encuesta <- svydesign(ids = ~WTSEG, weights = ~WEIGHT_WOMAN, data = institucional_36) 

resultados <- svyby(~factor(index_d_qoc), ~PAIS + FUP, diseno_encuesta, svymean)

resultados_proporciones <- resultados %>%
  dplyr::select(PAIS, FUP, `factor(index_d_qoc)0`, `factor(index_d_qoc)1`, `factor(index_d_qoc)2`, `factor(index_d_qoc)3`, `factor(index_d_qoc)4`) %>%
  rename(
    `0` = `factor(index_d_qoc)0`,
    `1` = `factor(index_d_qoc)1`,
    `2` = `factor(index_d_qoc)2`,
    `3` = `factor(index_d_qoc)3`,
    `4` = `factor(index_d_qoc)4`
  ) %>%
  pivot_longer(cols = `0`:`4`, names_to = "index_d_qoc", values_to = "proporcion")

resultados_proporciones <- resultados_proporciones %>%
  mutate(FUP = factor(FUP, levels = c("Baseline", "Second FUP", "Third FUP"))) %>% 
  mutate(index_d_qoc = factor(index_d_qoc, levels = c("4", "3", "2", "1", "0")))

conteo <- institucional_36 %>% 
  group_by(PAIS, FUP) %>%
  summarise(N_total = n())

final_grafico <- resultados_proporciones %>%
  left_join(conteo, by = c("PAIS", "FUP"))

aux_data <- final_grafico %>%
  group_by(PAIS, FUP) %>%
  slice(1) %>%  
  ungroup()

nombres_paises <- c("4 (4/4 affirmative domains)", "3 (3/4 affirmative domains)", "2 (2/4 affirmative domains)", "1 (1/4 affirmative domains)", "0 (0/4 affirmative domains)") 

#Fig 3. Proportion of institutional live births in each domain (quality score ranging from 0 to 4) across different countries and follow-up periods
graf_d <- ggplot(resultados_proporciones, aes(x = FUP, y = proporcion, fill = factor(index_d_qoc))) +
  geom_col(position = "dodge") +
  geom_text(data = aux_data, aes(x = FUP, y = 0.8, label = paste("N=", N_total, sep="")),
            position = position_dodge(width = 0.9), vjust = -0.5, size =3.5) +
  scale_y_continuous(limits = c(0, 0.9)) +
  facet_wrap(~ PAIS, ncol = 2, labeller = labeller(PAIS = function(x) c("Guatemala", "Honduras", "Chiapas", "Nicaragua"))) + 
  labs(title = "Score proportion distribution in live births by country and follow up",
       x = "Follow up", y = "Proportion of live births with each score", fill = "Quality domain score value") +
  scale_fill_manual(values = c("#BE2641", "#EA7246", "#E87687", "#76669B", "#33608C"),  # Ajusta los colores según tus preferencias
                    labels = nombres_paises) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 12)) +
  coord_flip()

print(graf_d)

#Home delivery

domiciliario <- dataset_unido %>%
  dplyr::filter(tipoparto == "domiciliario")

domiciliario <- domiciliario %>%
  dplyr::select(TX_AREA, PAIS, WEIGHT_WOMAN, WTSEG, FUP, starts_with("R_"))

domiciliario <- domiciliario %>%
  mutate(FUP = case_when(
    FUP == "BASELINE" ~ "Baseline",
    FUP == "36M" ~ "Second FUP",
    FUP == "54M" ~ "Third FUP",
    TRUE ~ FUP 
  ))

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
  dplyr::select(WTSEG, WEIGHT_WOMAN, Valor, TX_AREA, PAIS, FUP,)

domiciliario_apilado <- domiciliario_apilado %>% 
  dplyr::filter(Valor != "0")

diseno <- svydesign(
  id = ~WTSEG,         
  weights = ~WEIGHT_WOMAN,  
  data = domiciliario_apilado
)

tabla_proporciones <- svytable(~Valor+PAIS+FUP, design=diseno)

props <- prop.table(tabla_proporciones, margin=c(2,3)) * 100

print(props)

props_df <- as.data.frame.table(props)
names(props_df) <- c("Valor", "PAIS", "FUP", "Porcentaje")

nombres_dominios <- c("Knowledge domain", "Accesibility domain", "Infrastructure domain", "Treatment and Communication domain", "Family domain", "Cultural Preferences domain")

total_nacimientos <- dataset_unido %>%
  group_by(PAIS, FUP) %>%
  summarise(total_births_count = n(), .groups = 'drop')

total_nacimientos <- total_nacimientos %>% 
  mutate(
    FUP = case_when(
      FUP == "BASELINE" ~ "Baseline",
      FUP == "36M" ~ "Second FUP",
      FUP == "54M" ~ "Third FUP",
      TRUE ~ FUP ))

total_nacimientos <- total_nacimientos %>%
  mutate(FUP = factor(FUP, levels = c("Baseline", "Second FUP", "Third FUP")))

total_nacimientos <- total_nacimientos %>%
  mutate(PAIS = case_when(
    PAIS == "GTM" ~ "Guatemala",
    PAIS == "HND" ~ "Honduras",
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    TRUE ~ PAIS 
  ))

props_df <- props_df %>%
  mutate(PAIS = case_when(
    PAIS == "GTM" ~ "Guatemala",
    PAIS == "HND" ~ "Honduras",
    PAIS == "NIC" ~ "Nicaragua",
    PAIS == "MEX" ~ "Chiapas",
    TRUE ~ PAIS 
  ))

etiquetas_facet <- total_nacimientos %>%
  group_by(PAIS) %>%
  summarise(
    Baseline = total_births_count[FUP == "Baseline"][1],
    `Second FUP` = total_births_count[FUP == "Second FUP"][1],
    `Third FUP` = total_births_count[FUP == "Third FUP"][1],
    .groups = "drop"
  ) %>%
  mutate(
    etiqueta = case_when(
      !is.na(Baseline) & !is.na(`Second FUP`) & !is.na(`Third FUP`) ~
        paste0(PAIS, "\n",
               "Baseline: N=", Baseline,
               " | Second FUP: N=", `Second FUP`,
               " | Third FUP: N=", `Third FUP`),
      !is.na(Baseline) & !is.na(`Second FUP`) & is.na(`Third FUP`) ~
        paste0(PAIS, "\n",
               "Baseline: N=", Baseline,
               " | Second FUP: N=", `Second FUP`),
      TRUE ~ as.character(PAIS) 
    )
  ) %>%
  dplyr::select(PAIS, etiqueta)


etiquetas_map <- setNames(etiquetas_facet$etiqueta, etiquetas_facet$PAIS)

graf <- props_df %>%
  mutate(dominio_razon = case_when(
    Valor == "1" ~ "Knowledge domain",
    Valor == "2" ~ "Accesibility domain",
    Valor == "3" ~ "Infrastructure domain",
    Valor == "4" ~ "Treatment and Communication domain",
    Valor == "5" ~ "Family domain",
    Valor == "6" ~ "Cultural Preferences domain",
    TRUE ~ Valor
  )) %>%
  ggplot(aes(x = factor(FUP), y = Porcentaje, group = dominio_razon, color = dominio_razon)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ PAIS, labeller = labeller(PAIS = etiquetas_map)) +
  labs(
    title = "Factors influencing the birth delivery choices",
    x = "Follow up",
    y = "Percentage (%)",
    color = "Birth delivery choice factors"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))

print(graf)
#ggsave("Lineas.svg", plot = graf, width = 10, height = 8) 



####Figure 4
#### Institutional delivery
institucional <- dataset_unido %>%
  dplyr::filter(tipoparto == "institucional")

institucional_36 <- institucional %>%
  dplyr::select(TX_AREA, PAIS, WEIGHT_WOMAN, WTSEG, contains("Q_"), FUP)

############ LOOP FOR COMPLETE TABLE ###################
numeric_vars <- grep("Q_", colnames(institucional_36))
numeric_vars <- colnames(institucional_36)[numeric_vars]
append_df = data.frame()
resultados_completo = data.frame()
resultados_anova = data.frame()
areas = c("ALL","INTERVENTION","COMPARISON")
paises = c("HND","MEX","NIC","GTM")
FUP = c("BASELINE", "36M", "54M")

# Media calculation
for (l in FUP) {
  for (k in paises) {
    pais_sel = k
    for (i in areas) {
      data_institucional = institucional_36[institucional_36$FUP == l & is.na(institucional_36$TX_AREA) == F,]
      if (i == "ALL") {
        data_institucional = data_institucional[data_institucional$PAIS %in% pais_sel,]}
      else if (i== "INTERVENTION") {data_institucional = data_institucional[data_institucional$TX_AREA==1 & data_institucional$PAIS %in% pais_sel,]}
      else if (i== "COMPARISON") {
        data_institucional = data_institucional[data_institucional$TX_AREA==0 & data_institucional$PAIS %in% pais_sel,]}
      
      diseno <- svydesign(data = data_institucional, ids = ~WTSEG, weights = ~WEIGHT_WOMAN)
      qoc_36 <- c()
      qoc_name <- c()
      
      for (j in numeric_vars[numeric_vars!="Q_"]){
        
        # Media
        media_result <- svymean(as.formula(paste("~", j)), design = diseno, na.rm = TRUE)
        media_val <- coef(media_result)[1]
        
        # IC 95%
        intervalo_confianza <- confint(media_result)
        ic_inf <- intervalo_confianza[1, 1]
        ic_sup <- intervalo_confianza[1, 2]
        
        apend <- c(media_val, ic_inf, ic_sup)
        qoc_name <- c(qoc_name, paste(j, "_mean", sep = ""), paste(j, "_IC_lower", sep = ""), paste(j, "_IC_upper", sep = ""))
        qoc_36 <- c(qoc_36, apend)
      }
      
      append_df = data.frame(
        COUNTRY = k,
        FUP = l,
        AREA = i
      )
      append_df = cbind(append_df, as.data.frame(t(qoc_36)))
      
      resultados_completo = rbind(resultados_completo, append_df)
    }
  }
}

# Compare proportions between FUPs and countries (adjusted chi-square)
for (k in paises) {
  for (i in areas) {
    for (j in numeric_vars[numeric_vars!="Q_"]) {
      data_chisq <- institucional_36[institucional_36$PAIS == k &
                                       (i == "ALL" |
                                          (i == "INTERVENTION" & institucional_36$TX_AREA == 1) |
                                          (i == "COMPARISON" & institucional_36$TX_AREA == 0)), ]
      
      if (nrow(data_chisq) > 0 && length(unique(data_chisq$FUP)) > 1) {
        data_chisq[[j]] <- as.factor(data_chisq[[j]])
        diseno_chisq <- svydesign(data = data_chisq, ids = ~WTSEG, weights = ~WEIGHT_WOMAN)
        
        # Evaluar la tabla ponderada para verificar tamaños de celda
        tabla <- svytable(as.formula(paste("~", j, "+ FUP")), design = diseno_chisq)
        min_celda <- min(tabla)
        
        if (min_celda < 5) {
          warning(paste("Advertencia: celda con menos de 5 observaciones ponderadas para", j, "en", k, "-", i))
          test_apropiado <- "Se recomienda colapsar categorías o realizar un análisis descriptivo sin inferencia."
        } else {
          test_apropiado <- "svychisq"
        }
        
        chisq_test <- try(svychisq(as.formula(paste("~", j, "+ FUP")), design = diseno_chisq, statistic = "F"), silent = TRUE)
        
        if (!inherits(chisq_test, "try-error")) {
          resultado <- data.frame(
            COUNTRY = k,
            AREA = i,
            VARIABLE = j,
            F_statistic = chisq_test$statistic,
            df = paste(chisq_test$parameter, collapse = "/"),
            p_value = chisq_test$p.value,
            significant = ifelse(chisq_test$p.value < 0.05, "Yes", "No"),
            min_weighted_cell = min_celda,
            test_used = test_apropiado
          )
          resultados_anova <- rbind(resultados_anova, resultado)
        }
      }
    }
  }
}


# Save results
# writexl::write_xlsx(resultados_completo, "resultados_medias.csv")
# writexl::write_xlsx(resultados_anova, "resultados_anova.csv")


# 1) Visualization by country

names(resultados_completo) <- c("COUNTRY", "FUP", "AREA",
                                "Mean_Q_Treatment", "IC25_Q_Treatment", "IC975_Q_Treatment",
                                "Mean_Q_Communication", "IC25_Q_Communication", "IC975_Q_Communication",
                                "Mean_Q_Accompaniment", "IC25_Q_Accompaniment", "IC975_Q_Accompaniment",
                                "Mean_Q_Recommendedpr", "IC25_Q_Recommendedpr", "IC975_Q_Recommendedpr")


datos <- resultados_completo %>% 
  dplyr::filter(AREA == "ALL")

#Greed by country#

datos <- datos %>%
  dplyr::select(COUNTRY, FUP, 
                Mean_Q_Treatment, IC25_Q_Treatment, IC975_Q_Treatment,
                Mean_Q_Communication, IC25_Q_Communication, IC975_Q_Communication,
                Mean_Q_Accompaniment, IC25_Q_Accompaniment, IC975_Q_Accompaniment,
                Mean_Q_Recommendedpr, IC25_Q_Recommendedpr, IC975_Q_Recommendedpr) %>%
  dplyr::filter(Mean_Q_Treatment > 0 | Mean_Q_Communication > 0 | 
                  Mean_Q_Accompaniment > 0 | Mean_Q_Recommendedpr > 0) %>%
  pivot_longer(
    cols = c(starts_with("Mean_"), starts_with("IC25_"), starts_with("IC975_")),
    names_to = c(".value", "Domain"),
    names_pattern = "(.+)_Q_(.+)"
  )

datos <- datos %>%
  mutate(COUNTRY = case_when(
    COUNTRY == "NIC" ~ "Nicaragua",
    COUNTRY == "MEX" ~ "Chiapas",
    COUNTRY == "HND" ~ "Honduras",
    COUNTRY == "GTM" ~ "Guatemala",
    TRUE ~ COUNTRY 
  ))

datos <- datos %>%
  mutate(FUP = case_when(
    FUP == "BASELINE" ~ "Baseline",
    FUP == "36M" ~ "Second FUP",
    FUP == "54M" ~ "Third FUP",
    TRUE ~ FUP 
  ))

datos <- datos %>%
  mutate(Domain = case_when(
    Domain == "Recommendedpr" ~ "Recommended practices",
    TRUE ~ Domain 
  ))

# MEX to Chiapas
plot_pais <-  datos %>% 
  mutate(FUP = factor(FUP, levels = c("Baseline", "Second FUP", "Third FUP"))) %>% 
  ggplot(aes(x = Domain, y = Mean, fill = FUP)) +
  geom_bar(stat = "identity", position = "dodge") +  # Gráfico de barras agrupadas
  geom_errorbar(aes(ymin = IC25, ymax = IC975), position = position_dodge(width = 0.9), width = 0.2) +  # Barras de error
  facet_wrap(~ COUNTRY, ncol = 2) +  # Grilla de 4 gráficos (2 columnas)
  labs(
    title = "Proportion of affirmative answers in each country and each follow up for each quality domain",
    x = "Domain",
    y = "Proportion of affirmative answers",
    fill = "FUP"
  ) +
  scale_fill_manual(values = c("#4271AE", "#9B59B6", "#5DADE2")) +
  theme_bw() +  # Tema blanco y negro
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

print(plot_pais)


