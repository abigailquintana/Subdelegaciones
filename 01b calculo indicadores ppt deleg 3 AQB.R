library(pacman)
rm(list=ls())
p_load(here, tidyverse, readxl, scales,
       extrafont, grid, gtable,
       janitor, ggrepel, flextable)
# extrafont::font_import()

df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
         quirofanos, tipo, tipo_ordenado, clasificacion, rep) %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cons = case_when(tipo %in% c("HG", "CH",
                                                    "CEQ", "CMFEQ",
                                                    "CE") ~ "C. Hospital",
                                        tipo %in% c("CMF", "UMF") ~ "B. Unidad de consulta",
                                        tipo %in% c("CAF", "CMCT") ~ "A. Consultorio",
                                        TRUE ~ NA)) %>% 
  mutate(nombre_corto=str_replace(nombre_corto, "Ciudad de México Zona", "CDMX")) %>% 
  rename("clave_pres"=cve_pres) %>% 
  filter(!is.na(clave_pres))

rep_vect <- unique(df_cum$rep)[!is.na(unique(df_cum$rep))]

#### Consultas ####
load(file=here("01 datos", "df_consultas.RData"))
df_cons_abr24<- df_consultas %>% 
  filter(!is.na(clave))
df_cons_abr24 <- df_cons_abr24 %>%
  mutate(prom_cxconsultorio=promedio_mes,
         prom_cxconsultorio_ant=promedio23) %>% 
  rename("clave_pres" = clave,
         "consultas" = consultas_mes,
         "nombre_corto" = nombre_unidad,
         "tipo" = tipologia,
         "consultas_xconsult_abr24"= promedio_mes,
         "consultas_xconsult_23"= promedio23,
         "incremento"=cambio)  


df_cons_abr24<- df_cons_abr24 %>%
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX")) %>% 
  mutate(nombre_corto=str_replace(nombre_corto, "Ciudad de México Zona", "CDMX"))


df_cons_abr24 <- merge(df_cons_abr24, df_cum[, c("clave_pres", "clasificacion_cons")], 
                       by = "clave_pres", all.x = TRUE)


#### Tablas top consultas ####
get_top_bottom <- function(df) {
  df %>%
    arrange(desc(consultas_xconsult_abr24)) %>%
    mutate(
      consultas_xconsult_abr24 = round(consultas_xconsult_abr24, 1),  # Redondear consultas_xconsult_abr24
      rank = row_number()
    ) %>%
    filter(rank <= 5 | rank > n() - 5) %>%
    select(-rank)
}

top_bottom_5 <- df_cons_abr24 %>%
  group_by(clasificacion_cons) %>%
  group_modify(~ {
    top_bottom <- get_top_bottom(.x)
    top_bottom$clasificacion_cons <- unique(.x$clasificacion_cons)
    return(top_bottom)
  }) %>%
  ungroup() %>%
  select(clasificacion_cons, tipo, nombre_corto, rep, consultas_xconsult_abr24)

source(here("jalar_funciones.R"))

fx_tablas_cons <- function(data, clasif_actual, posic, suf_nombre) {
  filtered_data <- data %>%
    filter(clasificacion_cons == clasif_actual) %>%
    { if (posic == "top") head(., 5) else tail(., 5) } %>%
    select(tipo, nombre_corto, rep, consultas_xconsult_abr24) %>%
    rename(
      "Tipo" = tipo,
      "Nombre" = nombre_corto,
      "Delegación" = rep,
      "Prom. Con. x C." = consultas_xconsult_abr24
    )
  
  styled_data <- tema_flx1(filtered_data, color = "rojo") %>%
    width(j = 1, width = 0.6) %>%
    width(j = 2, width = 1.3) %>%
    width(j = 3, width = 1.3) %>%
    width(j = 4, width = 1) %>%
    fontsize(size = 10)
  
  save_as_image(styled_data, path = here("05 graficas del", paste0(suf_nombre, ".png")),
                width = 9.5 / 2.54, height = 5 / 2.54, dpi = 300)
}

clasif_actuales <- c("A. Consultorio", "B. Unidad de consulta", "C. Hospital")
posiciones <- c("top", "bottom")
sufijos <- list(
  "A. Consultorio" = c("con_alta_3", "con_baja_3"),
  "B. Unidad de consulta" = c("con_alta_2", "con_baja_2"),
  "C. Hospital" = c("con_alta_1", "con_baja_1")
)

for (clasif_actual in clasif_actuales) {
  for (posic in posiciones) {
    suf_nombre <- if (posic == "top") sufijos[[clasif_actual]][1] else sufijos[[clasif_actual]][2]
    fx_tablas_cons(top_bottom_5, clasif_actual, posic, suf_nombre)
  }
}


# Graficas promedio consulta por consultorio -----------------------------------
df_cons_abr24 <- mutate(df_cons_abr24,
                        nombre_corto = paste(consultorios, " C. | ", nombre_corto, sep=""),)

df_resumen <- df_cons_abr24 %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cons) %>% 
  summarise(top=max(consultas_xconsult_abr24, na.rm=T),
            bot=min(consultas_xconsult_abr24, na.rm=T),
            prom=mean(consultas_xconsult_abr24, na.rm=T),
            num=n())

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cons)) %>% 
  select(rep=Var1, clasificacion_cons=Var2) %>% 
  left_join(df_resumen) 

vec_clasif <- unique(df_resumen$clasificacion_cons)[!is.na(unique(df_resumen$clasificacion_cons))]

for(i in seq_along(vec_clasif)){
  estandar <- if_else(vec_clasif[i]=="A. Consultorio", 4*12,
                      if_else(vec_clasif[i]=="B. Unidad de consulta", 4*12,
                              if_else(vec_clasif[i]=="C. Hospital", 3*12, 0)))
  
  
  df_temp <- filter(df_resumen, clasificacion_cons==vec_clasif[i]) %>%  
    arrange(desc(prom), desc(num), rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_cons_abr24_seg <- df_cons_abr24 %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion_cons==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  prom_nal <- mean(df_cons_abr24_seg$consultas_xconsult_abr24, na.rm=T)
  
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  sd_nal <- sd(df_cons_abr24_seg$consultas_xconsult_abr24, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(prom >= prom_nal + sd_nal ~ "Muy alto",
                                                  prom >= prom_nal + 0.5 * sd_nal ~ "Alto",
                                                  prom >= prom_nal - 0.5 * sd_nal ~ "Medio",
                                                  prom >= prom_nal - sd_nal ~ "Bajo",
                                                  TRUE ~ "Muy bajo"),
                    colorear2 = case_when(prom >= prom_nal + sd_nal ~ "Muy alto 2",
                                          prom >= prom_nal + 0.5 * sd_nal ~ "Alto 2",
                                          prom >= prom_nal - 0.5 * sd_nal ~ "Medio 2",
                                          prom >= prom_nal - sd_nal ~ "Bajo 2",
                                          TRUE ~ "Muy bajo 2"))
  
  ggplot(df_temp, aes(x=rep2)) +
    geom_segment(aes(y=bot, yend=top, x=rep2, xend=rep2, color=colorear), size=4)+
    guides(color = "none") +
    geom_point(data=df_cons_abr24_seg, 
               aes(x=rep2, y=consultas_xconsult_abr24), alpha=0.2)+
    geom_segment(aes(y=prom-linea*4, yend=prom+linea*4, x=rep2,
                     xend=rep2, color=colorear2), size=7)+
    geom_segment(aes(y=prom-linea*3, yend=prom+linea*3, x=rep2,
                     xend=rep2), color="white", size=6)+
    geom_text(aes(x=rep2, y=prom, label=round(prom, 1)), size=2.5)+
    scale_color_manual(values = c("Muy alto" = "#a6d200", "Alto" = "#ccebc5", "Medio"="gray90",
                                  "Bajo" = "#fdcdac", "Muy bajo" = "#f4a582",
                                  "Muy alto 2" = "#33a02c", "Alto 2" = "#a6d200", "Medio 2"="gray60",
                                  "Bajo 2" = "#f4a582", "Muy bajo 2" = "#d73027"))+
    geom_hline(yintercept=estandar, linetype=2, color="#238443")+
    annotate("text", y=estandar-linea*3, x=unique(df_temp$rep2)[nrow(df_temp)-2],
             label=paste0("Estandar: ", estandar), color="#238443",
             family="Montserrat", fontface="bold")+
    geom_hline(yintercept=prom_nal, linetype=2, color="#54278f")+
    annotate("text", y=prom_nal+linea*3, x=unique(df_temp$rep2)[nrow(df_temp)-6],
             label=paste0("Promedio: ", round(prom_nal,1)), color="#54278f",
             family="Montserrat", fontface="bold")+
    theme_minimal() +
    labs(x = "", y = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold")
    ) 
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("consulta_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 5.8740157, dpi = 300)
  
}

# Grafica cambio porcentual ----------------------------------------------------
df_cons_abr24 <- mutate(df_cons_abr24,
                        nombre_corto = paste(consultorios, " C. | ", nombre_corto, sep=""),)

df_resumen <- df_cons_abr24 %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cons) %>% 
  summarise(top=max(consultas_xconsult_abr24, na.rm=T),
            bot=min(consultas_xconsult_abr24, na.rm=T),
            cambio=mean(cambio_cxconsultorio, na.rm=T),
            num=n()) 

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cons)) %>% 
  select(rep=Var1, clasificacion_cons=Var2) %>% 
  left_join(df_resumen) %>% 
  rename(clasificacion=clasificacion_cons)

vec_clasif <- unique(df_resumen$clasificacion)[!is.na(unique(df_resumen$clasificacion))]
vec_clasif<- "A. Consultorio" 

for(i in seq_along(vec_clasif)){
estandar <- if_else(vec_clasif[i]=="A. Consultorio", 4*12,
                    if_else(vec_clasif[i]=="B. Unidad de consulta", 4*12,
                            if_else(vec_clasif[i]=="C. Hospital", 3*12, 0)))


df_temp <- filter(df_resumen, clasificacion==vec_clasif[i]) %>%  
  arrange(desc(cambio), desc(num), rep) %>% 
  mutate(rep2=paste0(rep, " (", num, ")"))

df_cons_abr24_seg <- df_cons_abr24 %>% left_join(df_temp %>% select(rep, rep2)) %>% 
  filter(clasificacion==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))

order_nom_um <- df_temp %>%
  pull(rep2)
df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))

linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01


#sd_nal <- sd(df_cons_abr24_seg$consultas_xconsult_abr24, na.rm=T)
df_temp <- mutate(df_temp, colorear = case_when(cambio >= 0.15 ~ "Aumenta mucho",
                                                cambio > 0.05 & cambio < 0.15 ~ "Aumenta",
                                                cambio < -0.05 & cambio > -0.15 ~ "Disminuye",
                                                cambio <= -0.15 ~ "Disminuye mucho",
                                                TRUE ~ "Se mantiene"
                                                ),
                  posicion_text = case_when(cambio>=0 ~ 0.01,
                                            TRUE ~ -0.01),
                  posicion_text2 = case_when(cambio>=0 ~ 0,
                                             TRUE ~ 1))

lim_max <- max(df_temp$cambio, na.rm = TRUE)
lim_min <- min(df_temp$cambio, na.rm = TRUE)

if (lim_max < 1) {
  ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, lim_max)
} else {
  ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, 1)
}

ggplot(df_temp, aes(x = rep2, 
                    y = cambio,
                    fill = colorear)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = rep2, 
                y = posicion_text,
                hjust = posicion_text2,
                color = colorear, label=percent(cambio, accuracy = 0.1)), 
            size=4, fontface="bold", angle=90,
            family="Montserrat")+
  scale_fill_manual(values = c("Aumenta mucho" = "#a6d200", "Aumenta" = "#ccebc5",  "Promedio"="#807dba",
                               "Se mantiene" = "#D9D9D9", "Disminuye" = "#f4a582", "Disminuye mucho" = "#d73027")) +
  scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                "Se mantiene" = "black", "Disminuye" = "black",
                                "Disminuye mucho" = "black")) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(x="", y="Variación porcentual")+
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 13, color = "black"),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    plot.caption = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    text = element_text(family = "Montserrat"),
    strip.text.x = element_text(size = 14, face = "bold")
  ) +
  coord_cartesian(ylim = ylim)

ggsave(plot = last_plot(), 
       filename = here("05 graficas del", paste("diff_consulta_", vec_clasif[i],
                                                ".png", sep = "")), 
       width = 11, height = 6.8740157, dpi = 300)

}




#### Cirugias ####
load(here("01 datos", "df_cirugias.RData"))
df_cirugias <- filter(df_cirugias, !is.infinite(promedio_mes)) %>% 
  mutate(tipo=tipo_ordenado %>% 
           str_remove_all("^\\d*\\.\\s")) %>% select(-rep, -nombre, -ent_corta, -clasificacion)

df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
         tipo, tipo_ordenado, rep, clasificacion) %>%
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cir = case_when(tipo %in% c("HG", "CH") ~ "D. Hospital",
                                       tipo %in% c("CEQ", "CMFEQ", "CE") ~ "C. Cirugía ambulatoria",
                                       TRUE ~ NA)) %>% 
  mutate(nombre_corto=str_replace(nombre_corto, "Ciudad de México Zona", "CDMX")) 

df_cirugias <- left_join(df_cum %>% filter(tipo_ordenado > "6"), 
                         df_cirugias %>% select(-tipo_ordenado), 
                         by = c("cve_pres" = "clave", "tipo")) %>% 
  mutate(promedio_mes = case_when(
    is.na(promedio_mes) ~ 0,
    TRUE ~ round(promedio_mes, 1)  # Redondear a un decimal
  )) %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR")))

df_cirugias<- df_cirugias %>%
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX")) %>% 
  mutate(nombre_corto=str_replace(nombre_corto, "Ciudad de México Zona", "CDMX"))

#### Tablas top cirugias #### 
get_top_bottom <- function(df) {
  df %>%
    arrange(desc(promedio_mes)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 5 | rank > n() - 5) %>%
    select(-rank)
}


top_bottom_5 <- df_cirugias %>%
  group_by(clasificacion_cir) %>%
  group_modify(~ {
    top_bottom <- get_top_bottom(.x)
    top_bottom$clasificacion_cir <- unique(.x$clasificacion_cir)
    return(top_bottom)
  }) %>%
  ungroup() %>%
  select(clasificacion_cir, tipo, nombre_corto, rep, promedio_mes)

fx_tablas_cir <- function(data, clasif_actual, posic, suf_nombre) {
  filtered_data <- data %>%
    filter(clasificacion_cir == clasif_actual) %>%
    { if (posic == "top") head(., 5) else tail(., 5) } %>%
    select(tipo, nombre_corto, rep, promedio_mes) %>%
    rename(
      "Tipo" = tipo,
      "Nombre" = nombre_corto,
      "Delegación" = rep,
      "Prom. Cir x Q." = promedio_mes
    )
  
  styled_data <- tema_flx1(filtered_data, color = "rojo") %>%
    width(j = 1, width = 0.6) %>%
    width(j = 2, width = 1.3) %>%
    width(j = 3, width = 1.3) %>%
    width(j = 4, width = 0.8) %>%
    fontsize(size = 10)
  
  save_as_image(styled_data, path = here("05 graficas del", paste0(suf_nombre, ".png")),
                width = 9.5 / 2.54, height = 5 / 2.54, dpi = 300)
}

clasificaciones <- c("D. Hospital", "C. Cirugía ambulatoria")
posiciones <- c("top", "bottom")
sufijos <- list(
  "D. Hospital" = c("cir_alta_1", "cir_baja_1"),
  "C. Cirugía ambulatoria" = c("cir_alta_2", "cir_baja_2")
)

for (clasif_actual in clasificaciones) {
  for (posic in posiciones) {
    suf_nombre <- if (posic == "top") sufijos[[clasif_actual]][1] else sufijos[[clasif_actual]][2]
    fx_tablas_cir(top_bottom_5, clasif_actual, posic, suf_nombre)
  }
}


# Graficas promedio cirugias por quirofano -------------------------------------
df_resumen <- df_cirugias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cir) %>% 
  summarise(top=max(promedio_mes, na.rm=T),
            bot=min(promedio_mes, na.rm=T),
            prom=mean(promedio_mes, na.rm=T),
            num=n()) 

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cir)) %>% 
  select(rep=Var1, clasificacion_cir=Var2) %>% 
  left_join(df_resumen)

vec_clasif <- unique(df_resumen$clasificacion_cir)

for(i in seq_along(vec_clasif)){
  estandar <- 4
  
  df_temp <- filter(df_resumen, clasificacion_cir==vec_clasif[i]) %>%  
    arrange(desc(prom), desc(num), rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_cirugias_seg <- df_cirugias %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion_cir==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  prom_nal <- mean(df_cirugias_seg$promedio_mes, na.rm=T)
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  sd_nal <- sd(df_cirugias_seg$promedio_mes, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(prom >= prom_nal + sd_nal ~ "Muy alto",
                                                  prom >= prom_nal + 0.5 * sd_nal ~ "Alto",
                                                  prom >= prom_nal - 0.5 * sd_nal ~ "Medio",
                                                  prom >= prom_nal - sd_nal ~ "Bajo",
                                                  TRUE ~ "Muy bajo"),
                    colorear2 = case_when(prom >= prom_nal + sd_nal ~ "Muy alto 2",
                                          prom >= prom_nal + 0.5 * sd_nal ~ "Alto 2",
                                          prom >= prom_nal - 0.5 * sd_nal ~ "Medio 2",
                                          prom >= prom_nal - sd_nal ~ "Bajo 2",
                                          TRUE ~ "Muy bajo 2"))

  
  ggplot(df_temp, aes(x=rep2)) +
    geom_segment(aes(y=bot, yend=top, x=rep2, xend=rep2, color=colorear), size=4)+
    geom_point(data=df_cirugias_seg, 
               aes(x=rep2, y=promedio_mes), alpha=0.2)+
    geom_segment(aes(y=prom-linea*4, yend=prom+linea*4, x=rep2,
                     xend=rep2, color=colorear2), size=7)+
    geom_segment(aes(y=prom-linea*3, yend=prom+linea*3, x=rep2,
                     xend=rep2), color="white", size=6)+
    geom_text(aes(x=rep2, y=prom, label=round(prom, 1)), size=2.5)+
    scale_color_manual(values = c("Muy alto" = "#a6d200", "Alto" = "#ccebc5", "Medio"="gray90",
                                  "Bajo" = "#fdcdac", "Muy bajo" = "#f4a582",
                                  "Muy alto 2" = "#33a02c", "Alto 2" = "#a6d200", "Medio 2"="gray55",
                                  "Bajo 2" = "#f4a582", "Muy bajo 2" = "#d73027"))+
    guides(color = "none") +
    geom_hline(yintercept=estandar, linetype=2, color="#238443")+
    annotate("text", y=estandar-linea*3, x=unique(df_temp$rep2)[nrow(df_temp)-2],
             label=paste0("Estandar: ", estandar), color="#238443",
             family="Montserrat", fontface="bold")+
    geom_hline(yintercept=prom_nal, linetype=2, color="#54278f")+
    annotate("text", y=prom_nal+linea*3, x=unique(df_temp$rep2)[nrow(df_temp)-6],
             label=paste0("Promedio: ", round(prom_nal,1)), color="#54278f",
             family="Montserrat", fontface="bold")+
    theme_minimal() +
    labs(x = "", y = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold")
    ) 
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("cirugia_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 5.8740157, dpi = 300)
  
}


# Grafica cambio porcentual ----------------------------------------------------
df_resumen <- df_cirugias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cir) %>% 
  summarise(top=max(promedio_mes, na.rm=T),
            bot=min(promedio_mes, na.rm=T),
            cambio=mean(cambio_cxquirofano, na.rm=T),
            num=n()) 

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cir)) %>% 
  select(rep=Var1, clasificacion_cir=Var2) %>% 
  left_join(df_resumen) %>% 
  rename(clasificacion=clasificacion_cir)

vec_clasif <- unique(df_resumen$clasificacion)

for(i in seq_along(vec_clasif)){
  estandar <- if_else(vec_clasif[i]=="A. Consultorio", 4*12,
                      if_else(vec_clasif[i]=="B. Unidad de consulta", 4*12,
                              if_else(vec_clasif[i]=="C. Hospital", 3*12, 0)))
  
  
  df_temp <- filter(df_resumen, clasificacion==vec_clasif[i]) %>%  
    arrange(desc(cambio), desc(num), rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_cons_abr24_seg <- df_cons_abr24 %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  
  #sd_nal <- sd(df_cons_abr24_seg$consultas_xconsult_abr24, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(cambio >= 0.15 ~ "Aumenta mucho",
                                                  cambio > 0.05 & cambio < 0.15 ~ "Aumenta",
                                                  cambio < -0.05 & cambio > -0.15 ~ "Disminuye",
                                                  cambio <= -0.15 ~ "Disminuye mucho",
                                                  TRUE ~ "Se mantiene"
  ),
  posicion_text = case_when(cambio>=0 ~ 0.01,
                            TRUE ~ -0.01),
  posicion_text2 = case_when(cambio>=0 ~ 0,
                             TRUE ~ 1))
  
  lim_max <- max(df_temp$cambio, na.rm = TRUE)
  lim_min <- min(df_temp$cambio, na.rm = TRUE)
  
  if (lim_max < 1) {
    ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, lim_max)
  } else {
    ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, 1)
  }
  
  
  ggplot(df_temp, aes(x = rep2, 
                      y = cambio,
                      fill = colorear)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = rep2, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = colorear, label=paste(comma(cambio*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    scale_fill_manual(values = c("Aumenta mucho" = "#a6d200", "Aumenta" = "#ccebc5",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#f4a582", "Disminuye mucho" = "#d73027")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold")
    )+
    coord_cartesian(ylim = ylim)
    
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("diff_cirugias_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 6.8740157, dpi = 300)
  
}



#### Abasto ####
load(here("01 datos", "df_existencias.RData"))
df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, clasificacion, ent_corta, consultorios, 
         tipo, tipo_ordenado, rep) %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cons = case_when(tipo %in% c("HG", "CH",
                                                    "CEQ", "CMFEQ",
                                                    "CE") ~ "C. Hospital",
                                        tipo %in% c("CMF", "UMF") ~ "B. Unidad de consulta",
                                        tipo %in% c("CAF", "CMCT") ~ "A. Consultorio",
                                        TRUE ~ NA))

df_existencias <-  df_existencias %>% 
  mutate(
    rep = str_replace(rep, "Ciudad de México Zona", "CDMX"),
    tipo = tipologia,
    #proporcion0 = round(proporcion0, 1)  # Redondear proporcion0 con un decimal
  ) %>% 
  left_join(df_cum %>% select(cve_pres, clasificacion_cons), by = c("clave" = "cve_pres")) %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR")) & !is.na(proporcion0))
df_existencias<- df_existencias %>%
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX")) %>% 
  mutate(nombre_unidad=str_replace(nombre_unidad, "Ciudad de México Zona", "CDMX"))

#### Tablas top desabasto #### 

get_top_bottom <- function(df) {
  df %>%
    arrange(desc(proporcion0)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 5 | rank > n() - 5) %>%
    select(-rank)
}


top_bottom_5 <- df_existencias %>%
  group_by(clasificacion_cons) %>%
  group_modify(~ {
    top_bottom <- get_top_bottom(.x)
    top_bottom$clasificacion_cons <- unique(.x$clasificacion_cons)
    return(top_bottom)
  }) %>%
  ungroup() %>%
  select(clasificacion_cons,tipo, nombre_unidad, rep, proporcion0)

fx_desabasto <- function(data, clasif_actual, posic, suf_nombre) {
  filtered_data <- data %>%
    filter(clasificacion_cons == clasif_actual) %>%
    { if (posic == "top") head(., 5) else tail(., 5) } %>%
    select(tipo, nombre_unidad, rep, proporcion0) %>%
    rename(
      "Tipo" = tipo,
      "Nombre" = nombre_unidad,
      "Delegación" = rep,
      "% Cves. en 0" = proporcion0
    )
  
  styled_data <- tema_flx1(filtered_data, color = "rojo") %>%
    width(j = 1, width = 0.6) %>%
    width(j = 2, width = 1.3) %>%
    width(j = 3, width = 1.3) %>%
    width(j = 4, width = 0.8) %>%
    fontsize(size = 10)
  
  save_as_image(styled_data, path = here("05 graficas del", paste0(suf_nombre, ".png")),
                width = 9 / 2.54, height = 5 / 2.54, dpi = 300)
}

clasificaciones <- c("A. Consultorio", "B. Unidad de consulta", "C. Hospital")
posiciones <- c("top", "bottom")
sufijos <- list(
  "A. Consultorio" = c("desa_alta_3", "desa_baja_3"),
  "B. Unidad de consulta" = c("desa_alta_2", "desa_baja_2"),
  "C. Hospital" = c("desa_alta_1", "desa_baja_1")
)

for (clasif_actual in clasificaciones) {
  for (posic in posiciones) {
    suf_nombre <- if (posic == "top") sufijos[[clasif_actual]][1] else sufijos[[clasif_actual]][2]
    fx_desabasto(top_bottom_5, clasif_actual, posic, suf_nombre)
  }
}

# Grafica promedio de claves en cero -------------------------------------------
df_resumen <- df_existencias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cons) %>% 
  summarise(top=max(proporcion0, na.rm=T),
            bot=min(proporcion0, na.rm=T),
            prom=mean(proporcion0, na.rm=T),
            num=n()) 

df_tabla_desabasto <- select(df_resumen, rep, clasificacion_cons, prom) %>% 
  pivot_wider(names_from = clasificacion_cons, values_from = prom)

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cons)) %>% 
  select(rep=Var1, clasificacion_cons=Var2) %>% 
  left_join(df_resumen) %>% 
  filter(!is.na(clasificacion_cons))

vec_clasif <- unique(df_resumen$clasificacion_cons)

for(i in seq_along(vec_clasif)){
  estandar <- 0
  
  df_temp <- filter(df_resumen, clasificacion_cons==vec_clasif[i]) %>%  
    arrange(prom, desc(num), rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_existencias_seg <- df_existencias %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion_cons==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  prom_nal <- mean(df_existencias_seg$proporcion0, na.rm=T)
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  sd_nal <- sd(df_existencias_seg$proporcion0, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(prom >= prom_nal + sd_nal ~ "Muy alto",
                                                  prom >= prom_nal + 0.5 * sd_nal ~ "Alto",
                                                  prom >= prom_nal - 0.5 * sd_nal ~ "Medio",
                                                  prom >= prom_nal - sd_nal ~ "Bajo",
                                                  TRUE ~ "Muy bajo"),
                    colorear2 = case_when(prom >= prom_nal + sd_nal ~ "Muy alto 2",
                                          prom >= prom_nal + 0.5 * sd_nal ~ "Alto 2",
                                          prom >= prom_nal - 0.5 * sd_nal ~ "Medio 2",
                                          prom >= prom_nal - sd_nal ~ "Bajo 2",
                                          TRUE ~ "Muy bajo 2"))
  
  ggplot(df_temp, aes(x=rep2)) +
    geom_segment(aes(y=bot, yend=top, x=rep2, xend=rep2, color=colorear), size=4)+
    geom_point(data=df_existencias_seg, 
               aes(x=rep2, y=proporcion0), alpha=0.2)+
    geom_segment(aes(y=prom-linea*4, yend=prom+linea*4, x=rep2,
                     xend=rep2, color=colorear2), size=7)+
    geom_segment(aes(y=prom-linea*3, yend=prom+linea*3, x=rep2,
                     xend=rep2), color="white", size=6)+
    geom_text(aes(x=rep2, y=prom, label=percent(prom, accuracy = 1)), size=2.5)+
    scale_color_manual(values = c("Muy alto" = "#f4a582", "Alto" = "#fdcdac", "Medio"="gray90",
                                  "Bajo" = "#ccebc5", "Muy bajo" = "#a6d200",
                                  "Muy alto 2" = "#d73027", "Alto 2" = "#f4a582", "Medio 2"="gray60",
                                  "Bajo 2" = "#a6d200", "Muy bajo 2" = "#33a02c"))+
    guides(color = "none") +
    geom_hline(yintercept=estandar, linetype=2, color="#238443")+
    annotate("text", y=estandar-linea*3, x=unique(df_temp$rep2)[2],
             label=paste0("Estandar: ", estandar), color="#238443",
             family="Montserrat", fontface="bold")+
    geom_hline(yintercept=prom_nal, linetype=2, color="#54278f")+
    annotate("text", y=prom_nal+linea*3, x=unique(df_temp$rep2)[6],
             label=paste0("Promedio: ", percent(prom_nal, accuracy = 1)), color="#54278f",
             family="Montserrat", fontface="bold")+
    theme_minimal() +
    scale_y_continuous(labels=scales::percent) +
    labs(x = "", y = "") +
    #coord_cartesian(ylim = c(0, 20))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(
      axis.text = element_text(size = 13, color = "black"),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 12),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold")
    ) 
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("claves_cero_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 5.8740157, dpi = 300)
  
}

# Grafica cambio porcentual Claves cero ----------------------------------------
df_resumen <- df_existencias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cons) %>% 
  summarise(top=max(proporcion0, na.rm=T),
            bot=min(proporcion0, na.rm=T),
            cambio=mean(cambio_claves0, na.rm=T),
            num=n()) 

df_tabla_desabasto <- select(df_resumen, rep, clasificacion_cons, cambio) %>% 
  pivot_wider(names_from = clasificacion_cons, values_from = cambio)

df_resumen <- expand.grid(unique(df_cum$rep), unique(df_cum$clasificacion_cons)) %>% 
  select(rep=Var1, clasificacion_cons=Var2) %>% 
  left_join(df_resumen) %>% 
  rename(clasificacion=clasificacion_cons) %>% 
  filter(!is.na(rep),
         !is.na(clasificacion))

vec_clasif <- unique(df_resumen$clasificacion)

vec_clasif<-"B. Unidad de consulta"

for(i in seq_along(vec_clasif)){
  estandar <- if_else(vec_clasif[i]=="A. Consultorio", 4*12,
                      if_else(vec_clasif[i]=="B. Unidad de consulta", 4*12,
                              if_else(vec_clasif[i]=="C. Hospital", 3*12, 0)))
  
  
  df_temp <- filter(df_resumen, clasificacion==vec_clasif[i]) %>%  
    arrange(cambio, num, rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_cons_abr24_seg <- df_cons_abr24 %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  
  #sd_nal <- sd(df_cons_abr24_seg$consultas_xconsult_abr24, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(cambio >= 0.05 ~ "Aumenta mucho",
                                                  cambio >= 0.03 & cambio < 0.05 ~ "Aumenta",
                                                  cambio <= -0.03 & cambio > -0.2 ~ "Disminuye",
                                                  cambio <= -0.2 ~ "Disminuye mucho",
                                                  TRUE ~ "Se mantiene"
  ),
  posicion_text = case_when(cambio>=0 ~ 0.01,
                            TRUE ~ -0.01),
  posicion_text2 = case_when(cambio>=0 ~ 0,
                             TRUE ~ 1))
  
  lim_max <- max(df_temp$cambio, na.rm = TRUE)
  lim_min <- min(df_temp$cambio, na.rm = TRUE)
  
  if (lim_max < 1) {
    ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, lim_max)
  } else {
    ylim <- c(lim_min - (abs(lim_max) + abs(lim_min)) * 0.02, 1)
  }
  
  
  ggplot(df_temp, aes(x = rep2, 
                      y = cambio,
                      fill = colorear)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = rep2, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = colorear, label=paste(comma(cambio*100, accuracy = 0.1), sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    scale_fill_manual(values = c("Aumenta mucho" = "#d73027", "Aumenta" = "#f4a582",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#ccebc5", "Disminuye mucho" = "#a6d200")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels = function(x) x * 100)+
    #scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Puntos porcentuales")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 13, color = "black"),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 12),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold"))+
    coord_cartesian(ylim = ylim)
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("diff_claves0_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 6.8740157, dpi = 300)
  
}


# Grafica promedio de abasto ---------------------------------------------------
load(here("01 datos", "df_existencias.RData"))
df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, clasificacion, ent_corta, consultorios, 
         tipo, tipo_ordenado, rep) %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cons = case_when(tipo %in% c("HG", "CH",
                                                    "CEQ", "CMFEQ",
                                                    "CE") ~ "C. Hospital",
                                        tipo %in% c("CMF", "UMF") ~ "B. Unidad de consulta",
                                        tipo %in% c("CAF", "CMCT") ~ "A. Consultorio",
                                        TRUE ~ NA))

df_existencias <-  df_existencias %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         tipo=tipologia) %>% 
  left_join(df_cum %>% select(cve_pres, clasificacion_cons), by=c("clave"="cve_pres"))

df_resumen <- df_existencias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) 

prom_ent <- mean(df_resumen$prom_abasto, na.rm=T)
sd_ent <- sd(df_resumen$prom_abasto, na.rm=T)

df_resumen <- df_existencias %>% 
  group_by(rep) %>% 
  summarise(prom=mean(prom_abasto, na.rm=T),
            num=n()) 

df_resumen <- mutate(df_resumen, 
                     compara_prom=case_when(
                       prom > .98 ~ "Mucho mayor",
                       prom > .95  ~ "Mayor",
                       prom > .93  ~ "Igual",
                       prom > .90  ~ "Menor",
                       T ~ "Mucho menor"))

order_nom_um <- df_resumen %>%
  arrange(desc(prom)) %>% 
  pull(rep)
df_resumen$rep <- factor(df_resumen$rep, levels = unique(order_nom_um))

ggplot(df_resumen, aes(x = rep, 
                       y = prom,
                       fill = compara_prom)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = rep, 
                y = prom,
                color = compara_prom, label=paste0(comma(prom*100, accuracy = 0.1), "%")), 
            size = 2.5, fontface = "bold", vjust = 0) +
  geom_hline(yintercept=prom_ent, linetype=2, color="#54278f")+
  annotate("text", y=prom_ent+0.003, x="Puebla",
           label=paste0("Promedio: ", round(prom_ent*100, 2), "%"), color="#54278f",
           family="Montserrat", fontface="bold")+
  scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio" = "#807dba",
                               "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) +
  scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio" = "black",
                                "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
  theme_minimal() +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "", y = "Promedio de abasto diario") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(
    legend.title = element_blank(),
    legend.position = "none") +
  theme(
    axis.text = element_text(size = 13, color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 12),
    plot.caption = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    text = element_text(family = "Montserrat"),
    strip.text.x = element_text(size = 14, face = "bold")
  ) +
  coord_cartesian(ylim=c(min(df_resumen$prom, na.rm=T) - .015, 1))

ggsave(plot = last_plot(), filename = here("05 graficas del", "porcentaje_abasto.png"), 
       width = 11, height = 5.8740157, dpi = 300)


#Cambio porcentual de abasto ---------------------------------------------------
load(here("01 datos", "df_existencias.RData"))
df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, clasificacion, ent_corta, consultorios, 
         tipo, tipo_ordenado, rep) %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cons = case_when(tipo %in% c("HG", "CH",
                                                    "CEQ", "CMFEQ",
                                                    "CE") ~ "C. Hospital",
                                        tipo %in% c("CMF", "UMF") ~ "B. Unidad de consulta",
                                        tipo %in% c("CAF", "CMCT") ~ "A. Consultorio",
                                        TRUE ~ NA))

df_existencias <-  df_existencias %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         tipo=tipologia) %>% 
  left_join(df_cum %>% select(cve_pres, clasificacion_cons), by=c("clave"="cve_pres"))

df_resumen <- df_existencias %>% 
  filter(!is.na(rep) & !(tipo %in% c("CMN", "HR"))) 

df_resumen <- df_existencias %>% 
  group_by(rep) %>% 
  summarise(cambio=mean(cambio_abasto, na.rm=T),
            num=n()) 

df_temp <-df_resumen %>%  
  arrange(desc(cambio), desc(num), rep) %>% 
  mutate(rep2=paste0(rep, " (", num, ")"))

order_nom_um <- df_temp %>%
  pull(rep2)
df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))

linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01

df_temp <- mutate(df_temp, colorear = case_when(cambio >= 0.01 ~ "Aumenta mucho",
                                                cambio > 0.005 & cambio < 0.01 ~ "Aumenta",
                                                cambio < -0.005 & cambio >= -0.01 ~ "Disminuye",
                                                cambio <= -0.01 ~ "Disminuye mucho",
                                                TRUE ~ "Se mantiene"),
                  posicion_text = case_when(cambio>=0 ~ 0.001,
                                            TRUE ~ -0.01),
                  posicion_text2 = case_when(cambio>=0 ~ 0,
                                             TRUE ~ 0.1))



ggplot(df_temp, aes(x = rep2, 
                    y = cambio,
                    fill = colorear)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = rep2, 
                y = posicion_text,
                hjust = posicion_text2,
                color = colorear, label=paste(comma(cambio*100, accuracy = 0.1), sep="")), 
            size=4, fontface="bold", angle=90,
            family="Montserrat")+
  scale_fill_manual(values = c("Aumenta mucho" = "#a6d200", "Aumenta" = "#ccebc5",  "Promedio"="#807dba",
                               "Se mantiene" = "#D9D9D9", "Disminuye" = "#f4a582", "Disminuye mucho" = "#d73027")) +
  scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                "Se mantiene" = "black", "Disminuye" = "black",
                                "Disminuye mucho" = "black")) +
  scale_y_continuous(labels = function(x) x * 100)+
  #scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(x="", y="Puntos porcentuales")+
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 13, color = "black"),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 12),
    plot.caption = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    text = element_text(family = "Montserrat"),
    strip.text.x = element_text(size = 14, face = "bold"))

ggsave(plot = last_plot(), 
       filename = here("05 graficas del", paste("diff_abasto_", 
                                                ".png", sep = "")), 
       width = 11, height = 6.8740157, dpi = 300)




#### Uso de agenda ####
df_cirugias <- read_excel(here("01 datos", "uso de agenda 1Q mayo24.xlsx")) 
df_cirugias <- read_excel(here("01 datos", "Cirugías 16-30 may 2024.xlsx"))
names(df_cirugias)[1] <- "cve_pres"

df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
         tipo, tipo_ordenado, rep, clasificacion) %>% 
  mutate(rep=str_replace(rep, "Ciudad de México Zona", "CDMX"),
         clasificacion_cir = case_when(tipo %in% c("HG", "CH") ~ "D. Hospital",
                                       tipo %in% c("CEQ", "CMFEQ", "CE") ~ "C. Cirugía ambulatoria",
                                       TRUE ~ NA)) 


df_cirugias <- left_join(df_cirugias, df_cum, by=c("cve_pres")) 

df_resumen <- df_cirugias %>% 
  filter(!is.na(rep) & !is.na(clasificacion_cir) & !(tipo %in% c("CMN", "HR"))) %>% 
  group_by(rep, clasificacion_cir) %>% 
  summarise(top=max(promedio_mes, na.rm=T),
            bot=min(promedio_mes, na.rm=T),
            prom=mean(promedio_mes, na.rm=T),
            num=n()) %>% 
  filter(!is.nan(prom))

df_resumen <- expand.grid(unique(df_cum$rep)[!is.na(unique(df_cum$rep))], unique(df_cum$clasificacion_cir)) %>% 
  select(rep=Var1, clasificacion_cir=Var2) %>% 
  filter(!is.na(clasificacion_cir)) %>% 
  left_join(df_resumen) 

vec_clasif <- unique(df_resumen$clasificacion_cir)

for(i in seq_along(vec_clasif)){
  
  df_temp <- filter(df_resumen, clasificacion_cir==vec_clasif[i]) %>%  
    arrange(desc(prom), desc(num), rep) %>% 
    mutate(rep2=paste0(rep, " (", num, ")"))
  
  df_cirugias_seg <- df_cirugias %>% left_join(df_temp %>% select(rep, rep2)) %>% 
    filter(clasificacion_cir==vec_clasif[i] & !is.na(rep2) & !(tipo %in% c("CMN", "HR")))
  
  prom_nal <- mean(df_cirugias_seg$promedio_mes, na.rm=T)
  
  order_nom_um <- df_temp %>%
    pull(rep2)
  df_temp$rep2 <- factor(df_temp$rep2, levels = unique(order_nom_um))
  
  linea <- (max(df_temp$top, na.rm=T) - min(df_temp$bot, na.rm=T))*0.01
  
  sd_nal <- sd(df_cirugias_seg$promedio_mes, na.rm=T)
  df_temp <- mutate(df_temp, colorear = case_when(prom >= prom_nal + sd_nal ~ "Muy alto",
                                                  prom >= prom_nal + 0.5 * sd_nal ~ "Alto",
                                                  prom >= prom_nal - 0.5 * sd_nal ~ "Medio",
                                                  prom >= prom_nal - sd_nal ~ "Bajo",
                                                  TRUE ~ "Muy bajo"),
                    colorear2 = case_when(prom >= prom_nal + sd_nal ~ "Muy alto 2",
                                          prom >= prom_nal + 0.5 * sd_nal ~ "Alto 2",
                                          prom >= prom_nal - 0.5 * sd_nal ~ "Medio 2",
                                          prom >= prom_nal - sd_nal ~ "Bajo 2",
                                          TRUE ~ "Muy bajo 2"))
  
  ggplot(df_temp, aes(x=rep2)) +
    geom_segment(aes(y=bot, yend=top, x=rep2, xend=rep2, color=colorear), size=4)+
    geom_point(data=df_cirugias_seg, 
               aes(x=rep2, y=promedio_mes), alpha=0.2)+
    geom_segment(aes(y=prom-linea*4, yend=prom+linea*4, x=rep2,
                     xend=rep2, color=colorear2), size=7)+
    geom_segment(aes(y=prom-linea*3, yend=prom+linea*3, x=rep2,
                     xend=rep2), color="white", size=6)+
    geom_text(aes(x=rep2, y=prom, label=round(prom*100, 1)), size=2.5)+
    scale_color_manual(values = c("Muy alto" = "#a6d200", "Alto" = "#ccebc5", "Medio"="gray90",
                                  "Bajo" = "#fdcdac", "Muy bajo" = "#f4a582",
                                  "Muy alto 2" = "#33a02c", "Alto 2" = "#66bd63", "Medio 2"="gray60",
                                  "Bajo 2" = "#f4a582", "Muy bajo 2" = "#d73027"))+
    guides(color = "none") +
    geom_hline(yintercept=prom_nal, linetype=2, color="#54278f")+
    annotate("text", y=prom_nal+linea*3, x=unique(df_temp$rep2)[nrow(df_temp)-6],
             label=paste0("Promedio: ", round(prom_nal*100,1)), color="#54278f",
             family="Montserrat", fontface="bold")+
    theme_minimal() +
    scale_y_continuous(labels=scales::percent)+
    labs(x = "", y = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(
      axis.text = element_text(size = 13, color = "black"),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size = 14, face = "bold")
    ) 
  
  ggsave(plot = last_plot(), 
         filename = here("05 graficas del", paste("prog quir_", vec_clasif[i],
                                                  ".png", sep = "")), 
         width = 11, height = 5.8740157, dpi = 300)
  
}
