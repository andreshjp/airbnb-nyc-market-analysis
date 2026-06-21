# ============================================================
# financial-fraud-detection
# Author: Andrés Jiménez | github.com/andreshjp
# ============================================================

# Paquetes necesarios:
library(readr)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)
library(ggplot2)
library(treemapify)
library(reshape2)

# Importar csv a RStudio, colocarle nombre sencillo y renombrar columna "availability_365;;;;;"
original <- read_csv("New York City Airbnb Open Data (1).csv") %>% 
  rename("availability_365" = "availability_365;;;;;")

# Filtrar la columna "id" para crear un dataset que solo  muestre los que tienen id numérico
good.id <- original %>% 
  filter(grepl("^[0-9]+$", id))
         
# Filtrar la columna "id" para crear un dataset que solo muestre los que no tienen id numérico y reemplazar las "," por "|" del name para poder separar por columnas
wrong.id<-original %>%
  filter(!grepl("^[0-9]+$", id)) %>% 
  mutate(id = str_replace_all(id, '(?<=\").*?(?=\")', function(x) str_replace_all(x, ',', '|')))

# Separar la columna "id" en las columnas correspondientes usando el delimitador ","  
wrong.id <- wrong.id %>% 
  separate(id, into = c("id", "name", "host_id", "host_name", "neighbourhood_group", 
                        "neighbourhood", "latitude", "longitude", "room_type", "price", 
                        "minimum_nights", "number_of_reviews", "last_review", 
                        "reviews_per_month", "calculated_host_listings_count", 
                        "availability_365"), sep = ",", extra = "merge", fill = "right")

# Unir toda la data que se limpió
clean.ish <- rbind(good.id, wrong.id)

# Quitar los ";" de la columna "availability_365" y cambiar de la columna "name" los "|" a "," nueavmente
clean.ish <- clean.ish %>%
  mutate(availability_365 = gsub(";", "", availability_365),
         name = gsub("\\|", ",", name))

# Verificar que la columna "id" sea toda numérica creando un dataset con aquellos que no lo son
nonumeric.id <- clean.ish %>%
  filter(!grepl("^[0-9]+$", id))

# Quitar de la data todas las observaciones que no sean numéricas en la columna "id"
clean.ish <- clean.ish %>%
  filter(grepl("^[0-9]+$", id))

# Identificar las filas que 3 o mas columnas tengan NA y borrarlas
Data.na <- rowSums(is.na(clean.ish)) >= 3

# Eliminar las filas identificadas
Data <- clean.ish[!Data.na, ]

# Ya con la data limpia procedemos a hacer las operaciones, identificamos que las clases sean las correctas.
sapply(Data, class)

# Se cambian aquellas para que correspondan a la clase correcta
Data <- Data %>%
  mutate(across(c(availability_365, price, latitude, longitude, reviews_per_month, number_of_reviews), as.numeric),
         across(c(id, minimum_nights, host_id, calculated_host_listings_count), as.integer),
         across(c(neighbourhood_group, room_type), as.factor))

# Ahora sí podemos preceder a obtener información de los datos

# 1. ¿Cómo se distribuyen las habitaciones entre los condados?
Data %>% 
  count(neighbourhood_group) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Tree map en representación del total de habitaciones
plot1 <- Data %>% 
  count(neighbourhood_group) %>%
  mutate(percentage = (n / sum(n)) * 100) %>% 
  mutate(label = paste0(neighbourhood_group, " (", round(percentage, 1), "%)")) %>%
  ggplot(aes(area = n, fill = neighbourhood_group, label = label)) +
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre") +
  scale_fill_manual(values = c("#986f70", "#ff5a5f","#dc6164","#767676","#ba696a" ))+
  labs(title = "Cantidad de Airbnbs por Condado en Nueva York")
#Guardar el plot1 como imagen
ggsave("total_neighbourhood_group.jpg", plot1, width = 12, height = 8, bg = "white", dpi = 300)

# 2. ¿Cuál condado tiene más vecindarios?
Data %>%
  group_by(neighbourhood_group) %>% 
  summarise(num_neighbourhoods = n_distinct(neighbourhood))

# 3. ¿Qué relación existe entre el precio y los condados?
plot2<-ggplot(Data, aes(x = neighbourhood_group, y = price, fill = neighbourhood_group)) +  
  # Añadir fill dentro de aes()
  geom_boxplot() +
  scale_fill_manual(values = c("#986f70", "#ff5a5f","#dc6164","#767676","#ba696a")) +
  theme_light() +
  labs(
    title = "Distribución de Precios por Condados",
    x = "Condados",
    y = "Precio (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_cartesian(ylim = c(0, quantile(Data$price, 0.95, na.rm = TRUE)))
# Guardar el plot2 como imagen
ggsave("price_by_neighbourhood.jpg", plot2, width = 10, height = 6, bg = "white")

# Resumen de Neighbourhood
Data %>%
  group_by(neighbourhood_group) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    n = n())

# 4. ¿Qué relación existe entre el precio y el tipo de habitación?
plot3 <- ggplot(Data, aes(x = room_type, y = price, fill= room_type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#986f70", "#ff5a5f","#dc6164")) +
  theme_light() +
  labs(
    title = "Distribución de Precios por Tipo de Habitación",
    x = "Tipo de Habitación",
    y = "Precio (USD)",
    ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_cartesian(ylim = c(0, quantile(Data$price, 0.95, na.rm = TRUE)))
# Guardar el plot3 como imagen
ggsave("price_by_roomtype.jpg", plot3, width = 10, height = 6, bg = "white")
# Resumen de tipo de habitación
room_type_summary <- Data %>%
  group_by(room_type) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    n = n())


# 5. ¿Qué relación existe entre el precio y la disponibilidad?
# Crear una nueva columna para categorizar la disponibilidad de los Airbnb
Data <- Data %>%
  mutate(
    availability_category = case_when(
      availability_365 <= 0 ~ "not available",
      availability_365 <= 30 ~ "1 month or less",
      availability_365 <= 90 ~ "1-3 months",
      availability_365 <= 180 ~ "3-6 months",
      availability_365 <= 270 ~ "6-9 months",
      availability_365 <= 365 ~ "9-12 months")
    ) %>%
  mutate(availability_category = factor(availability_category, 
                                        levels = c("not available","1 month or less", "1-3 months", "3-6 months", "6-9 months", "9-12 months")))
# Calcular los precios promedios de cada categoria
avg_prices <- Data %>%
  group_by(availability_category) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    n_listings = n(),
    se = sd(price, na.rm = TRUE) / sqrt(n()))
# Crear grafico para visualizar mejor los precios promedios por categoria
plot4 <- ggplot(avg_prices, aes(x = availability_category, y = avg_price, fill = availability_category)) + # fill dentro de aes()
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = avg_price - se, ymax = avg_price + se),
                width = 0.2, color = "#946f70") +
  geom_text(aes(label = paste0("n = ", n_listings, "\n$", round(avg_price, 0))),
            position = position_dodge(width = 0.7),
            vjust = -0.8, size = 4) +
  theme_light() +
  labs(
    title = "Precio promedio por periodo disponible",
    x = "Periodo disponible",
    y = "Precio Promedio (USD)",
    fill = "Disponibilidad",
    ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    limits = c(0, max(avg_prices$avg_price + avg_prices$se) * 1.15)
  ) +
  scale_fill_manual(values = c("#857273", "#a36c6e", "#b2696b", "#d16366", "#ff5a5f","#ef5d61"))
# Guardar el plot4 como imagen
ggsave("price_vs_availability.jpg", plot4, width = 12, height = 8, bg = "white", dpi = 300)
# Correlacion de Availability y Price
correlation <- cor(Data$availability_365, Data$price, use = "complete.obs")
  print(sprintf("Correlation between price and availability: %.5f", correlation))


#6.¿Qué relación hay entre los condados y la disponibilidad?
availability_vs_neighbourhood <- Data %>%
  group_by(neighbourhood_group, availability_category) %>%
  summarise(cantidad = n())
# Crear grafico para visualizar mejor los periodos de disponibilidad por vecindario
plot5 <- ggplot(availability_vs_neighbourhood, aes(x = neighbourhood_group, y = availability_category, fill = cantidad)) +
  geom_tile() +  
  scale_fill_gradient(low = "white", high = "#FF5A5F") +  
  labs(title = "Heat Map Availability Period by Neighbourhood Group ",
       x = "Neighbourhood Group",
       y = "Availability Period ",
       fill = "Quantity") +
  theme_bw() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Guardar el plot5 como imagen
ggsave("neighbourhood_group_vs_availability2.jpg", plot5, width = 12, height = 8, bg = "white", dpi = 300)


# 7. ¿Qué relación tienen las reseñas?
# 7.1 Con el tiempo
Data$year_review <- as.factor(format(Data$last_review, "%Y"))

reviews_by_year <- Data %>%
  group_by(year_review) %>%
  summarise(count_rv_year = sum(!is.na(neighbourhood_group)))

plot6 <- ggplot(reviews_by_year, aes(x = year_review, y = count_rv_year, fill = count_rv_year)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#FFB5B8", high = "#FF5A5F") +
  labs(title = "Conteo de Reviews por Año",
       x = "Año de la reseña",
       y = "Número de Reviews") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
# Guardar el plot6 como imagen
ggsave("reviews_by_year.jpg", plot6, width = 12, height = 8, bg = "white", dpi = 300)

# 7.2 Con los condados
neighbourhood_reviews <- Data %>% 
  group_by(neighbourhood_group) %>% 
  summarise(
    reviews = sum(!is.na(year_review)),
    NA_s = sum(is.na(year_review)))

neighbourhood_reviews_total <- melt(neighbourhood_reviews,
                                   id.vars = "neighbourhood_group",
                                   variable.name = "type",
                                   value.name = "count")

plot7 <- ggplot(neighbourhood_reviews_total, aes(x = neighbourhood_group, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Número de Reviews y NAs por Condados",
       x = "Condado",
       y = "Count",
       fill = "Type") +  
  scale_fill_manual(values = c("#FF5A5F", "#FFB5B8")) + 
  theme_bw()
# Guardar el plot7 como imagen
ggsave("reviews_by_neighbourhood.jpg", plot7, width = 12, height = 8, bg = "white", dpi = 300)

#7.3 Con los condados y con el tiempo
neighbourhood_year_review<-Data %>%
  group_by(neighbourhood_group, year_review) %>%
  filter(!is.na(year_review)) %>% 
  summarise(count=n())

plot8 <- ggplot(neighbourhood_year_review, aes(x = year_review, y = count, fill = neighbourhood_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de Reseñas por Año y Condado",
       x = "Año de la reseña",
       y = "Número de reseñas",
       fill = "Condados") + 
  scale_fill_manual(values = c("#986f70", "#ff5a5f","#dc6164","#767676","#ba696a" )) +
  theme_bw()
# Guardar el plot8 como imagen
ggsave("reviews_by_neighbourhood_year.jpg", plot8, width = 12, height = 8, bg = "white", dpi = 300)

# 8. ¿Cuál es la estancia mínima promedio entre condados?
Data %>%
  group_by(neighbourhood_group) %>%
  summarise(n=n(),promedio_minimo = mean(minimum_nights, na.rm = TRUE))

# Calcula la tabla de estancia mínima promedio entre condados
min_stay_table <- Data %>%
  group_by(neighbourhood_group) %>%
  summarise(n = n(), promedio_minimo = mean(minimum_nights, na.rm = TRUE))

# Crea el gráfico de barras con fondo blanco
plot17 <- ggplot(min_stay_table, aes(x = neighbourhood_group, y = promedio_minimo, fill = neighbourhood_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(promedio_minimo, 2)), vjust = -0.5, color = "black", size = 3) +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Estancia Mínima Promedio entre Condados",
       x = "Condado",
       y = "Estancia Mínima Promedio (días)",
       fill = "Condado") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot17.png", plot = plot17, bg = "white", width = 8, height = 6, dpi = 300)


# Adentrándonos en los condados más significativos (Manhattan y Brooklyn)

# 9. ¿Cómo son los precios de los tipos de habitación de Manhattan y Brooklyn?
Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, room_type) %>% 
  summarise(n = n(), mean(price))

# Calcula la tabla de precios por tipo de habitación
price_table <- Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(n = n(), mean_price = mean(price))

# Crea el gráfico de barras con fondo blanco
plot9 <- ggplot(price_table, aes(x = room_type, y = mean_price, fill = neighbourhood_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5, color = "black", position = position_dodge(0.9), size = 3) +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Precios Promedio de los Tipos de Habitación en Manhattan y Brooklyn",
       x = "Tipo de Habitación",
       y = "Precio Promedio ($)",
       fill = "Vecindario") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot9.png", plot = plot9, bg = "white", width = 8, height = 6, dpi = 300)



# 10. ¿Qué tipos de habitaciones hay en Manhattan y Brooklyn?
Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, room_type) %>% 
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de tipos de habitaciones
room_type_table <- Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de barras apiladas con fondo blanco y etiquetas de datos
plot10 <- ggplot(room_type_table, aes(x = neighbourhood_group, y = percentage, fill = room_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Tipos de Habitaciones en Manhattan y Brooklyn",
       x = "Vecindario",
       y = "Porcentaje",
       fill = "Tipo de Habitación") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot10.png", plot = plot10, bg = "white", width = 8, height = 6, dpi = 300)


# 11. ¿Cómo es la disponibilidad en Manhattan y Brooklyn?
Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, availability_category) %>% 
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de disponibilidad
availability_table <- Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, availability_category) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de barras apiladas con fondo blanco y etiquetas de datos
plot11 <- ggplot(availability_table, aes(x = neighbourhood_group, y = percentage, fill = availability_category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Disponibilidad en Manhattan y Brooklyn",
       x = "Vecindario",
       y = "Porcentaje",
       fill = "Categoría de Disponibilidad") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot11.png", plot = plot11, bg = "white", width = 8, height = 6, dpi = 300)
  
# 12. ¿Qué cantidad de reviews por año hay en Manhattan y Brooklyn?
Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, year_review) %>% 
  filter(!is.na(year_review)) %>% 
  summarise(n=n()) %>% 
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de reviews por año
reviews_table <- Data %>%
  filter(neighbourhood_group %in% c("Manhattan", "Brooklyn")) %>%
  group_by(neighbourhood_group, year_review) %>%
  filter(!is.na(year_review)) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de líneas con los colores especificados
plot12 <- ggplot(reviews_table, aes(x = as.numeric(year_review), y = n, color = neighbourhood_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Cantidad de Reviews por Año en Manhattan y Brooklyn",
       x = "Año",
       y = "Cantidad de Reviews",
       color = "Vecindario") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot12.png", plot = plot12, bg = "white", width = 8, height = 6, dpi = 300)


# Ahora analizando los condados menos significativos (Bronx, Staten Island y Queens)

# 13. ¿Cómo son los precios de los tipos de habitación de Bronx, Staten Island y Queens?
Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, room_type) %>% 
  summarise(n = n(), mean(price))

# Calcula la tabla de precios por tipo de habitación
price_table <- Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(n = n(), mean_price = mean(price))

# Crea el gráfico de barras con fondo blanco y etiquetas de datos reducidas
plot13 <- ggplot(price_table, aes(x = room_type, y = mean_price, fill = neighbourhood_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_price, 2)), vjust = -0.5, color = "black", position = position_dodge(0.9), size = 3) +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164")) +
  labs(title = "Precios Promedio de los Tipos de Habitación en Bronx, Staten Island y Queens",
       x = "Tipo de Habitación",
       y = "Precio Promedio ($)",
       fill = "Vecindario") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot13.png", plot = plot13, bg = "white", width = 8, height = 6, dpi = 300)

# 14. ¿Qué tipos de habitaciones hay en Bronx, Staten Island y Queens?
Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, room_type) %>% 
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de tipos de habitaciones
room_type_table <- Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de barras apiladas con fondo blanco y etiquetas de datos
plot14 <- ggplot(room_type_table, aes(x = neighbourhood_group, y = percentage, fill = room_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Tipos de Habitaciones en Bronx, Staten Island y Queens",
       x = "Vecindario",
       y = "Porcentaje",
       fill = "Tipo de Habitación") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot14.png", plot = plot14, bg = "white", width = 8, height = 6, dpi = 300)


# 15. ¿Cómo es la disponibilidad en Bronx, Staten Island y Queens?
Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, availability_category) %>% 
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de disponibilidad
availability_table <- Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, availability_category) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de barras apiladas con fondo blanco y etiquetas de datos
plot15 <- ggplot(availability_table, aes(x = neighbourhood_group, y = percentage, fill = availability_category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5), color = "white") +
  scale_fill_manual(values = c("#986f70", "#ff5a5f", "#dc6164", "#767676", "#ba696a", "#c94c4c")) +
  labs(title = "Disponibilidad en Bronx, Staten Island y Queens",
       x = "Vecindario",
       y = "Porcentaje",
       fill = "Categoría de Disponibilidad") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot15.png", plot = plot15, bg = "white", width = 8, height = 6, dpi = 300)


# 16. ¿Qué cantidad de reviews por año hay en Bronx, Staten Island y Queens?
Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, year_review) %>% 
  filter(!is.na(year_review)) %>% 
  summarise(n=n()) %>% 
  mutate(percentage = (n / sum(n)) * 100)

# Calcula la tabla de reviews por año
reviews_table <- Data %>%
  filter(neighbourhood_group %in% c("Bronx", "Staten Island", "Queens")) %>%
  group_by(neighbourhood_group, year_review) %>%
  filter(!is.na(year_review)) %>%
  summarise(n = n()) %>%
  mutate(percentage = (n / sum(n)) * 100)

# Crea el gráfico de líneas con los colores especificados
plot16 <- ggplot(reviews_table, aes(x = as.numeric(year_review), y = n, color = neighbourhood_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#986f70", "#ff5a5f", "#dc6164")) +
  labs(title = "Cantidad de Reviews por Año en Bronx, Staten Island y Queens",
       x = "Año",
       y = "Cantidad de Reviews",
       color = "Vecindario") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

# Guarda el gráfico como imagen
ggsave("plot16.png", plot = plot16, bg = "white", width = 8, height = 6, dpi = 300)
