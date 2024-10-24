#plots for univariate analysis

library(ggplot2)
library(tidyr)

# Dictionary for Spanish item descriptions
item_labels <- list(
  # Frequency
  fuse1 = "Búsqueda de información académica",
  fuse2 = "Explicación de temas complejos",
  fuse3 = "Revisión gramátical y ortográfica",
  fuse4 = "Producción de textos académicos",
  fuse5 = "Traducción de textos",
  fuse6 = "Asuntos personales",
  fuse7 = "Entretenimiento o curiosidad",
  
  # Personal use
  puse1 = "Uso IA en tareas académicas",
  puse2 = "Uso IA en estudio/investigación",
  puse3 = "Uso IA para problemas personales",
  puse4 = "Uso IA para intereses personales",
  
  # Others use
  ouse1 = "Estudiantes usan IA académicamente",
  ouse2 = "Uso estudiantil se vuelve norma",
  ouse3 = "Profesores usan IA",
  ouse4 = "Profesores integran IA en docencia",
  
  # Personal benefits
  pben1 = "Mejora calidad de trabajos",
  pben2 = "Ayuda a ser mejor profesional",
  pben3 = "Mejora rendimiento académico",
  pben4 = "Facilita aprendizaje conceptual",
  
  # Personal risks
  pris1 = "Daña desarrollo personal",
  pris2 = "Riesgo para seguridad en internet",
  pris3 = "Podría reemplazarme laboralmente",
  pris4 = "Riesgos personales en general",
  
  # Social benefits
  sben1 = "Resuelve problemas globales",
  sben2 = "Construye mejor sociedad",
  sben3 = "Aporta al desarrollo científico",
  sben4 = "Facilita innovación en áreas",
  
  # Social risks
  sris1 = "Amenaza existencial humana",
  sris2 = "Puede dañar a las personas",
  sris3 = "Riesgo de estafas",
  sris4 = "Riesgo de desinformación",
  
  # Knowledge
  know1 = "Sé usar herramientas IA",
  know2 = "Entiendo principios básicos",
  know3 = "Me cuesta entender funcionamiento",
  know4 = "Me resulta difícil usar IA",
  
  # Skepticism
  skep1 = "Se exagera impacto real",
  skep2 = "Es solo una moda",
  skep3 = "Ya supera inteligencia humana",
  skep4 = "Potencial incomprensible",
  
  # Education future
  edfu1 = "Apoyo enseñanza sobre IA",
  edfu2 = "Futuro incluye integración IA",
  edfu3 = "Universidad debe evitar IA",
  edfu4 = "Mantener educación sin IA",
  
  # Ethics
  ethi1 = "Uso estudiantil es ético",
  ethi2 = "Uso docente es ético",
  ethi3 = "Uso es deshonesto",
  ethi4 = "Es una forma de hacer trampa"
)


# Function to create plots with different color schemes for freq vs likert
create_scale_plot <- function(data, vars, title, is_frequency = FALSE) {
  # Prepare data
  plot_data <- data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), 
                 names_to = "item", 
                 values_to = "response") %>%
    filter(!is.na(response)) %>%  # Remove NAs
    mutate(
      response = factor(response, 
                        levels = if(is_frequency) 0:4 
                        else c(5,4,3,2,1),  # Changed order here
                        labels = if(is_frequency) 
                          c("Nunca", "Semestral", "Mensual", "Semanal", "Diario")
                        else 
                          c("Muy de\nacuerdo", "De\nacuerdo", "Neutral", 
                            "En\ndesacuerdo", "Muy en\ndesacuerdo")),  # Changed order here
      item = factor(item, 
                    levels = vars,
                    labels = item_labels[vars])
    )
  
  # Define colors based on scale type
  if (is_frequency) {
    colors <- c("#0B8FB1", "#45ACC5", "#7FC8D9", "#BBE5ED", "#F5F5F5")  # Blue intensity
  } else {
    colors <- c("#D46666", "#EEAAAA", "#E8E8E8", "#A8D6B0", "#5BB075")  # Red to Green, order matches factor levels
  }
  
  # Invert levels for legend
  legend_levels <- if (is_frequency) 
    c("Diario", "Semanal", "Mensual", "Semestral", "Nunca") 
  else 
    c("Muy en\ndesacuerdo", "En\ndesacuerdo", "Neutral", 
      "De\nacuerdo", "Muy de\nacuerdo")
  
  # Create plot
  ggplot(plot_data, aes(y = item, fill = response)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = colors, limits = legend_levels) +  # Invert legend levels here
    scale_x_continuous(labels = scales::percent) +
    labs(title = title,
         x = "Proporción",
         y = "",
         fill = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 11),
          axis.text.y = element_text(size = 9),  # Smaller for longer labels
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
}

# Create plots for each subscale
create_scale_plot(adf, paste0("fuse", 1:7), "Frecuencia de uso", TRUE)
create_scale_plot(adf, paste0("puse", 1:4), "Uso personal")
create_scale_plot(adf, paste0("ouse", 1:4), "Uso percibido en otros")
create_scale_plot(adf, paste0("pben", 1:4), "Beneficios personales")
create_scale_plot(adf, paste0("pris", 1:4), "Riesgos personales")
create_scale_plot(adf, paste0("sben", 1:4), "Beneficios sociales")
create_scale_plot(adf, paste0("sris", 1:4), "Riesgos sociales")
create_scale_plot(adf, paste0("know", 1:4), "Conocimiento")
create_scale_plot(adf, paste0("skep", 1:4), "Escepticismo")
create_scale_plot(adf, paste0("edfu", 1:4), "Futuro educativo")
create_scale_plot(adf, paste0("ethi", 1:4), "Ética")