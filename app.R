#install.packages("slickR")
#install.packages("shinyBS")
#install.packages("fontawesome")
library(shiny)
library(shinythemes)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(corrplot)
library(liver)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(DT)
library(tidyr)
library(shinydashboard)
library(stringr) 
library(caret)
library(cluster)
library(factoextra)
library(reshape2)
library(shinythemes)
library(shinyBS)  
library(arules)
library(arulesViz)
library(visNetwork)
library(lubridate)

#CARGAMOS DATOS

data <- read.csv('bakery_sales_revised.csv', sep = ",", header = TRUE)

#Exploracion de los datos

str(data)

data_summary <- summary(data)
data_str <- capture.output(str(data))
data_table <- datatable(data)

#Separar los datos de fecha y hora
data <- data %>%
  separate(date_time, into = c("Fecha", "Hora"), sep = "\\s+")  # \\s+ para un o más espacios
# Convertir 'Fecha' y 'Hora' a formato de fecha y hora
data$Fecha <- as.POSIXct(data$Fecha, format = "%m/%d/%Y")



# Extraer Mes y Día
data <- data %>%
  mutate(
    Mes = month(Fecha, label = TRUE, abbr = FALSE),  # Extraer mes como nombre completo
    Dia = day(Fecha)  # Extraer el día
  )

data <- data %>%
  mutate(Hora = as.numeric(sub(":.*", "", Hora)))

# Reemplazar nombres de horas
data <- data %>%
  mutate(Hora = case_when(
    Hora %in% 1 ~ "1-2",
    Hora %in% 2 ~ "2-3",
    Hora %in% 3 ~ "3-4",
    Hora %in% 4 ~ "4-5",
    Hora %in% 5 ~ "5-6",
    Hora %in% 6 ~ "6-7",
    Hora %in% 7 ~ "7-8",
    Hora %in% 8 ~ "8-9",
    Hora %in% 9 ~ "9-10",
    Hora %in% 10 ~ "10-11",
    Hora %in% 11 ~ "11-12",
    Hora %in% 12 ~ "12-13",
    Hora %in% 13 ~ "13-14",
    Hora %in% 14 ~ "14-15",
    Hora %in% 15 ~ "15-16",
    Hora %in% 16 ~ "16-17",
    Hora %in% 17 ~ "17-18",
    Hora %in% 18 ~ "18-19",
    Hora %in% 19 ~ "19-20",
    Hora %in% 20 ~ "20-21",
    Hora %in% 21 ~ "21-22",
    Hora %in% 22 ~ "22-23",
    Hora %in% 23 ~ "23-24",
    TRUE ~ as.character(Hora) 
  ))

# Reemplazar nombres de días de la semana
data <- data %>%
  mutate(Dia_Semana = wday(Fecha, label = TRUE, abbr = FALSE)) 

color <- c(
  "#A4D8E1",  # Cyan claro
  "#4DA6C4",  # Cyan medio
  "#2E93B8",  # Cyan oscuro
  "#1C7A92",  # Cyan muy oscuro
  "#0B4462" , # Cyan profundo,
  "#DAF7A6",  # Verde claro,
  "#FF6F61",  # Coral
  "#F7CAC9",  # Rosa claro
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#88B04B" ,# Verde manzana
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#82CCDD",  # Aqua
  "#C5E1A5",  # Verde claro (Mate)
  "#FFB142",  # Naranja suave
  "#BFACE2"   # Lavanda

)

mis_colores <- c("#0B4462", "#DAF7A6","#82CCDD",  "#FFB142")

datos_finales<-data %>%
  rename(
    Articulo= Item,
    Periodo_del_dia=period_day,
    DiadeSem_FindeSem=weekday_weekend
    
  )

# Esto es solo para visualizar las variables finales, despues no lo uso 

final_dataset<-datos_finales

# Función para crear gráficos de barras
barrasFrec <- function(datos, column, color) {
  plot_title <- paste('Frecuencia de:', column)
  
  # Frecuencia de cada categoría
  filtered_data <- datos %>%
    count(!!sym(column)) %>%
    mutate(Percentage = n / sum(n) * 100)
  
  
  if (column == "Articulo") {
    filtered_data <- filtered_data %>%
      slice_max(n, n = 10, with_ties = FALSE)
  }
  if (column == "Hora") {
    filtered_data <- filtered_data %>%
      slice_max(n, n = 10, with_ties = FALSE)
  }
  
  # Gráfico
  barFrec <- plot_ly(
    filtered_data,
    x = ~filtered_data[[column]],  # Utilizar la columna como x
    y = ~Percentage,               # Utilizar el porcentaje como y
    type = 'bar',
    text = ~paste0(round(Percentage, 2), "%"),  # Texto con porcentaje
    textposition = 'auto',
    marker = list(color = color)
  ) %>%
    layout(
      title = plot_title,
      xaxis = list(title = column),
      yaxis = list(title = "Frecuencia (%)")
    )
  
  return(barFrec)
}

barItem<-barrasFrec(datos_finales,"Articulo",color)
barperioday<-barrasFrec(datos_finales,"Periodo_del_dia",color)
barweekday <- barrasFrec(datos_finales, "DiadeSem_FindeSem", color)

barday <- barrasFrec(datos_finales, "Dia_Semana", color)
barmes<- barrasFrec(datos_finales, "Mes",color)
barhora<- barrasFrec(datos_finales, "Hora",color)

#segundo grafico 
# Agrupar por 'weekday_weekend' y 'period_day' y contar las transacciones
trans_summary <- datos_finales %>%
  group_by(DiadeSem_FindeSem, Periodo_del_dia) %>%
  summarise(cantidad_transacciones = n(), .groups = 'drop')  # Contar transacciones

# Crear gráfico de columnas apiladas
g4<- ggplot(trans_summary, aes(x = DiadeSem_FindeSem, 
                               y = cantidad_transacciones, 
                               fill = Periodo_del_dia)) +
  geom_col(position = "stack") +  # Apilar las columnas
  theme_minimal() +  
  labs(
       x = "Día de la Semana (Weekday o Weekend)", 
       y = "Cantidad de Transacciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = mis_colores)


apiladas <- ggplotly(g4)

#### Tercer grafico
# Función para generar el gráfico basado en el periodo del día seleccionado
generarColumnPlot <- function(period_day_seleccionado, data, color) {
  # Filtrar los datos según el periodo del día seleccionado
  datos_filtrados <- subset(data, period_day == period_day_seleccionado)
  
  # Contar la cantidad de transacciones por ítem
  top_items <- datos_filtrados %>%
    group_by(Item) %>%
    summarise(Cantidad = n()) %>%
    arrange(desc(Cantidad)) %>%
    head(10)
  
  # Crear el gráfico de columnas
  column_plot <- ggplot(top_items, aes(x = Item, y = Cantidad, fill = Item)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
         x = "Item",
         y = "Cantidad") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    scale_fill_manual(values = color)
  
  # Convertir a gráfico interactivo con plotly
  return(ggplotly(column_plot))
}

###Cuarto grafico

generarHeatmap <- function(datos, periodo_seleccionado, mes_seleccionado) {
  
  # Filtrar los datos según el periodo del día y el mes seleccionados
  datos_filtrados <- datos %>%
    filter(Periodo_del_dia == periodo_seleccionado, Mes == mes_seleccionado)
  
  # Contar el número de transacciones por día de la semana y periodo del día
  heatmap_data <- datos_filtrados %>%
    count(Dia_Semana, Periodo_del_dia)
  
  # Crear el gráfico de heatmap
  heatmap <- ggplot(heatmap_data, aes(x = Periodo_del_dia, y = Dia_Semana, fill = n)) +
    geom_tile() +
    scale_fill_gradient(low =  "#DAF7A6", high ="#0B4462") +
    theme_minimal() +
    labs(x = "Hora", 
         y = "Día de la Semana",
         fill = "Número de Ventas")
  
  # Convertir a gráfico interactivo con plotly
  return(ggplotly(heatmap))
}

##Quinto grafico

# Filtrar los 10 artículos más vendidos
top_items <- datos_finales %>%
  count(Articulo) %>%
  slice_max(n, n = 10)

# Gráfico de dona
dona_chart <- plot_ly(
  top_items, 
  labels = ~Articulo, 
  values = ~n, 
  type = 'pie', 
  hole = 0.5,
  textinfo = 'label+percent',
  marker = list(colors = color)
)



#----------Algoritmo APRIORI----------------------------------------------------------------------

# Seleccionar columnas y eliminar duplicados usando unique
datos <- unique(data[, c("Transaction", "Item")])

# Crear un objeto transacción
transacciones_arules <- as(split(datos$Item, datos$Transaction), "transactions")
summary(transacciones_arules)

# Generar reglas de asociación
rules_apriori <- apriori(transacciones_arules, parameter = list(supp = 0.01, conf = 0.5, target = "rules" ))

# Ordenar las reglas por lift de forma descendente
rules_apriori <- sort(rules_apriori, by = "lift", decreasing = TRUE)

# Mostrar las 10 mejores reglas
top_10_rules <- head(rules_apriori, n = 10)
inspect(top_10_rules)
top_10_rules_df <- as(top_10_rules, "data.frame")

#---------Algoritmo FP-Growth --------------------------------------------------------------------
transacciones_fp<-transacciones_arules


# Generar los itemsets frecuentes usando FP-Growth
itemsets_frecuentes <- apriori(transacciones_fp, parameter = list(supp = 0.01, target = "frequent itemsets"))

# Mostrar los itemsets frecuentes
inspect(itemsets_frecuentes)

# Generar reglas de asociación a partir de los itemsets frecuentes
rules_fp <- apriori(transacciones_fp, parameter = list(supp = 0.01, conf = 0.5, target = "rules"))

# Mostrar las reglas
inspect(rules_fp)





#------------------------------------------------ui-----------------------------------#
ui <- dashboardPage(
  dashboardHeader(title = "Reglas de asociación"),
  
  dashboardSidebar(
    tags$div(
      style = "color: #ffffff; font-weight: bold; font-size: 18px; font-family: Arial, sans-serif;",
      
      sidebarMenu(
        menuItem('Teoría', tabName='teoria', icon=icon('book'),
                 menuSubItem('Información',tabName ='teoria',icon = icon("lightbulb")),
                 menuSubItem('Algoritmos',tabName ='algoritmos',icon = icon("cogs"))),
        menuItem('Datos', tabName='datos', icon=icon('database'),
                 menuSubItem('RMD', tabName='rmarkdown', icon=icon('file-text')),
                 menuSubItem('Dataset', tabName='dataset', icon=icon('table'))),
        menuItem('EDA', tabName='eda', icon=icon('search')),
        menuItem('Reglas de asociacion', tabName='reglas', icon=icon('layer-group')),
        menuItem('Simulacion', tabName="simulacion", icon=icon('project-diagram'))
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    body, h1, h2, h3, h4, h5, h6 {
      font-family: 'Arial', sans-serif; /* Cambia la fuente a Arial */
    }

    /* Cambiar estilo para los encabezados de todos los boxes */
    .box-header {
      background-color: #6DB3C8 !important;  /* Color de fondo del header */
      color: #333 !important;                /* Color del texto */
      padding: 10px !important;              /* Espaciado dentro del header */
      border-radius: 5px 5px 0 0 !important; /* Bordes redondeados solo arriba */
    }
    
     .box-custom {
        height: 300px; /* Ajusta la altura según lo necesario */
        overflow-y: auto; /* Permitir desplazamiento si el contenido es demasiado largo */
      }

    /* Eliminar el borde de los boxes */
    .box {
      border: none !important;
      box-shadow: none !important;
    }

    /* Eliminar el borde inferior del header de los boxes */
    .box-header {
      border-bottom: none !important;
    }

    /* Cambiar el color de fondo y el borde superior */
    .main-header .navbar {
      background-color: #6DB3C8 !important; /* Color de fondo de la barra superior */
    }

    /* Cambiar el color de fondo de la barra superior */
    .skin-blue .main-header .logo {
      background-color: #6DB3C8 !important;  /* Cambia el color de la parte izquierda */
    }

    /* Cambiar color del menú hamburguesa (las 3 rayitas) al hacer hover */
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
      background-color: #6DB3C8 !important;  /* Cambia el hover de las tres rayitas */
      color: white !important;  /* Cambia el color del icono de las tres rayitas */
    }

    /* Cambiar la rayita azul al costado del ítem del menú seleccionado */
    .skin-blue .main-sidebar .sidebar-menu > li.active > a {
      border-left-color: #ADD8E6 !important;  /* Cambia la rayita azul del ítem seleccionado */
    }

    /* Cambiar el color de los ítems del menú al pasar el mouse */
    .skin-blue .main-sidebar .sidebar-menu > li > a:hover {
      background-color: #6DB3C8 !important;  /* Cambia el color de fondo al hacer hover en el menú */
      color: white !important;  /* Cambia el color del texto en hover */
    }

    /* Fondo animado */
    @keyframes backgroundAnimation {
      0% { background: linear-gradient(45deg, rgba(173, 216, 230, 0.5), rgba(135, 206, 235, 0.5)); }
      100% { background: linear-gradient(45deg, rgba(135, 206, 235, 0.5), rgba(173, 216, 230, 0.5)); }
    }
    
    .animated-background {
      background-image: url('supermercado.jpg'); /* Imagen en la carpeta www */
      background-size: cover; /* Cubre todo el fondo */
      background-position: center; /* Centra la imagen */
      height: 100vh; /* Ocupa toda la altura de la pantalla */
      width: 100vw; /* Ocupa toda la anchura de la pantalla */
      position: absolute; /* Asegura que el fondo cubra toda la pantalla */
      top: 0;
      left: 0;
      z-index: -1; /* Para que no interfiera con otros elementos */
    }

    .content {
      position: relative; /* Permite que el contenido se muestre sobre el fondo */
      z-index: 1;
      color: #333; /* Color del texto principal */
      padding: 20px; /* Añade algo de espacio */
    }

    /* Estilo específico para el box en el panel Interactivo */
    .interactive-box {
      background-color: rgba(249, 249, 249, 0.9); /* Fondo suave */
      border-radius: 5px; /* Bordes redondeados */
    }

    .large-input-container {
      font-size: 30px;  /* Aumenta el tamaño de la fuente */
      height: 50px;  /* Aumenta la altura del contenedor */
    }
    
    .large-input-container input {
      font-size: 30px;  /* Aumenta el tamaño de la fuente del input */
      height: 50px;  /* Aumenta la altura del input */
    }
    
    $(document).ready(function() {
      $('a').hover(
        function() {
          $('#message').text($(this).attr('title')); // Cambiar el texto al pasar el mouse
        }, 
        function() {
          $('#message').text(''); // Limpiar el texto al salir
        }
      );
    });

  "))
    )
  ,
    
    
    tabItems(
      
      
      
      tabItem(tabName = 'teoria',
              titlePanel('Teoría de Reglas de Asociación'),
              fluidRow(
                box(width = 12,
                    tags$h4("Reglas de Asociación", style = "margin-top: 20px; margin-bottom: 10px;"),
                    tags$p("Las reglas de asociación son herramientas fundamentales en el análisis de datos que permiten descubrir patrones o relaciones interesantes entre objetos o atributos en grandes conjuntos de datos, especialmente a partir de bases de datos transaccionales.", style = "margin-bottom: 10px;"),
                    tags$p("Es una técnica de inteligencia artificial ampliamente utilizada en Data Mining", style = "margin-bottom: 10px;"),
                    tags$br(),
                    tags$p("Una regla de asociación se puede expresar en la forma X ⟹ Y, donde X e Y son conjuntos disjuntos de ítems. Esto significa que si se encuentran todos los ítems en X en una transacción, existe una probabilidad de encontrar también los ítems en Y con un nivel de confianza determinado. Esto nos permite identificar relaciones entre variables cualitativas. Es importante destacar que esta relación implica coocurrencia, no causalidad, es decir, no necesariamente una causa lleva a la otra.", style = "margin-bottom: 20px;"),
                    tags$br(), 
                    tags$h4("Aplicaciones de las Reglas de Asociación", style = "margin-top: 20px; margin-bottom: 10px;"),
                    bsCollapse(
                      bsCollapsePanel("Análisis de la Cesta de la Compra", 
                                      tags$p("Este tipo de análisis se utiliza para identificar asociaciones entre productos comprados. Por ejemplo, una regla como {pan} ⟹ {leche} sugiere que la compra de pan frecuentemente se acompaña de la compra de leche.", style = "margin-bottom: 10px;")
                      ),
                      bsCollapsePanel("Análisis de Textos", 
                                      tags$p("Las reglas de asociación se pueden aplicar para identificar patrones en textos, como términos que aparecen juntos en documentos.", style = "margin-bottom: 10px;")
                      ),
                      bsCollapsePanel("Identificación de Patrones en Páginas Web", 
                                      tags$p("Se utiliza el análisis de consultas para adaptar la interfaz de las páginas web a la actividad del usuario, mejorando así la experiencia de navegación.", style = "margin-bottom: 10px;")
                      ),
                      bsCollapsePanel("Bioinformática y Diagnóstico Médico", 
                                      tags$p("En el ámbito de la bioinformática, las reglas de asociación ayudan a identificar patrones en datos genéticos o clínicos que pueden ser relevantes para el diagnóstico médico.", style = "margin-bottom: 10px;")
                      )
                    ),
                    tags$br(),  # Espacio adicional
                    tags$h4("Definiciones Importantes", style = "margin-top: 20px; margin-bottom: 10px;"),
                    tabsetPanel(
                      tabPanel("Ítems",
                               tags$br(),
                               tags$p("Los ítems son los artículos que componen una transacción. Por ejemplo, en una venta, los productos comprados constituyen los ítems.", style = "margin-bottom: 10px;")
                      ),
                      tabPanel("Itemset",
                               tags$br(),
                               tags$p("Un itemset es un conjunto de ítems de una transacción. Un k-itemset es un itemset que contiene k artículos. Ejemplos incluyen productos en la cesta de la compra o páginas web visitadas por un usuario.", style = "margin-bottom: 10px;")
                      ),
                      tabPanel("Base de Datos Transaccional",
                               tags$br(),
                               tags$p("Una base de datos transaccional es un conjunto de transacciones, denotadas como T = {t1, t2, ..., tN}. Cada transacción se representa por su conjunto de ítems, denotados como I = {i1, i2, ..., id}.", style = "margin-bottom: 10px;")
                      )
                    ),
                    tags$br(),  # Espacio adicional
                    tags$h4("Parámetros Clave en el Análisis", style = "margin-top: 20px; margin-bottom: 10px;"),
                    tabsetPanel(
                      tabPanel("Soporte", 
                               tags$br(),
                               tags$p("El soporte de un itemset X se define como la fracción de transacciones que incluyen dicho itemset. Matemáticamente, se expresa como:", style = "margin-bottom: 10px;"),
                               img(src='soporte.png', style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Donde N es el número total de transacciones en la base de datos y count(X ∪ Y) es el número de transacciones que contienen todos los ítems en X (antecedente) o Y (consecuente).", style = "margin-bottom: 10px;"),
                               img(src='example.png',style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Interpretación: regla con bajo soporte: puede haber aparecido por casualidad", style = "margin-bottom: 10px;")
                      ),
                      tabPanel("Confianza",
                               tags$br(),  # Espacio adicional
                               tags$p("La confianza de una regla se define como la fracción de transacciones que contienen tanto X como Y. Esto se puede expresar como:", style = "margin-bottom: 10px;"),
                               img(src='confianza.png', style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Es decir, mide cuántas veces los ítems en Y aparecen cuando los ítems en X están presentes.", style = "margin-bottom: 10px;"),
                               img(src='example2.png',style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Interpretación: regla con baja confianza: es probable que no exista relación entre antecedente y consecuente", style = "margin-bottom: 10px;")
                      ),
                      
                      tabPanel("Elevación/Lift",
                               tags$br(),  # Espacio adicional
                               tags$p("Es un indicador que ayuda a entender la fuerza de la relación entre dos ítems en comparación con su ocurrencia independiente. Se utiliza para evaluar la efectividad de una regla de asociación y se define como:", style = "margin-bottom: 10px;"),
                               img(src='lift.png', style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Donde: ", style = "margin-bottom: 10px;"),
                               tags$p("P(X∩Y) es la probabilidad de que los ítems 𝑋 y 𝑌 ocurran juntos.", style = "margin-bottom: 10px;"),
                               tags$p("P(X) es la probabilidad de que el ítem X ocurra.", style = "margin-bottom: 10px;"),
                               tags$p("P(Y) es la probabilidad de que el ítem Y ocurra.", style = "margin-bottom: 10px;"),
                               img(src='example3.png',style = "width: 40%; height: auto; margin-bottom: 20px;"),
                               tags$p("Interpretación: Lift > 1: Indica que los ítems X y𝑌 ocurren juntos con más frecuencia de lo que se esperaría si fueran independientes. Esto sugiere una asociación positiva entre los ítems", style = "margin-bottom: 10px;"),
                               tags$p("Interpretación: Lift < 1: Indica que los ítems X y𝑌 ocurren juntos con menos frecuencia de lo que se esperaría si fueran independientes. Esto sugiere una asociación negativa entre los ítems", style = "margin-bottom: 10px;")
                      ),
                      tabPanel("Proceso de Descubrimiento de Reglas",
                               tags$br(),
                               tags$p("Para descubrir reglas de asociación efectivas, es necesario establecer límites mínimos de soporte y confianza. Un itemset se considera frecuente si su soporte supera un umbral mínimo. El proceso de búsqueda de reglas de asociación se puede dividir en dos pasos:", style = "margin-bottom: 10px;"),
                               tags$p("1. Detectar itemsets frecuentes que superen un soporte mínimo, es decir, cuya ocurrencia exceda un número mínimo de transacciones.", style = "margin-bottom: 10px;"),
                               tags$p("2. Obtener las reglas de asociación asociadas a estos itemsets que cumplan con un nivel de confianza establecido.", style = "margin-bottom: 10px;")
                      )
                    )
                )
              )
      ),

      # 4to tabItem: Algoritmos
      tabItem(tabName='algoritmos',
              titlePanel('Algoritmos'),
              fluidRow(
                box(width = 12,
                    tabsetPanel(
                      tabPanel("Algoritmo Apriori", 
                               tags$br(), 
                               tags$p("El principio Apriori establece que todos los subconjuntos no vacíos de un itemset frecuentes, también son frecuentes. Por ejemplo, si el itemset {b, c} es frecuente, puedo suponer por ejemplo que {b} es también frecuente (como mínimo aparecerá el mismo número de veces que {b, c}). De igual forma, cualquier adición de items a {b, c} también será frecuente:"),
                               img(src = 'apriori.png', style = "width:100%;height: auto; margin-bottom: 20px;"),
                               tags$p("Por otro lado, si {c, d} es infrecuente, {b, c, d} tampoco será frecuente. Esta propiedad se conoce como antimonotonicidad, y permite hacer una poda en el espacio de búsqueda basada en el soporte: ", style = "margin-bottom: 10px;"),
                               img(src = 'apriori2.png', style = "width:100%;height: auto; margin-bottom: 20px;")
                      ),
                      # Segundo Tab Panel: Funcionamiento del Algoritmo Apriori
                      tabPanel("Funcionamiento del Algoritmo Apriori",
                               tags$br(), 
                               tags$p("El algoritmo Apriori sigue estos pasos básicos:"),
                               tags$p( "1. Generar Conjuntos de Ítems Frecuentes: ",
                                 tagList(
                                   tags$ul(
                                     tags$li("Paso 1: Identificar los ítems en las transacciones."),
                                     tags$li("Paso 2: Calcular el soporte de cada ítem."),
                                     tags$li("Paso 3: Seleccionar ítems que cumplen con el soporte mínimo."),
                                     tags$li("Paso 4: Generar conjuntos de ítems frecuentes.")))),
                               tags$p( "2. Crear Nuevos Conjuntos de Ítems: ",
                                       tagList(
                                         tags$ul(
                                           tags$li("Se combinan los ítems frecuentes para formar conjuntos de ítems de mayor tamaño (por ejemplo, de 1 ítem a 2 ítems, de 2 ítems a 3 ítems, etc.)."),
                                           tags$li("Se repite el proceso de cálculo de soporte y se eliminan aquellos conjuntos que no cumplen con el umbral mínimo.")))),
                               tags$p( "3. Generar Reglas de Asociación:",
                                       tagList(
                                         tags$ul(
                                           tags$li("A partir de los conjuntos de ítems frecuentes, se generan reglas de asociación."),
                                           tags$li("Se calcula la confianza para cada regla y se seleccionan aquellas que cumplen con un umbral mínimo de confianza.")))),
                               tags$p( "4. Filtrar Reglas: ",
                                       tagList(
                                         tags$ul(
                                           tags$li("Se pueden filtrar las reglas en función del lift u otras métricas para obtener las más relevantes o interesantes."))))),
                      #Tercer subpanel 
                     tabPanel("Algoritmo FP-Growth (Frequent Pattern Growth)", 
                                       tags$br(), 
                                        tags$p("Es un método eficiente para encontrar conjuntos de ítems frecuentes en grandes bases de datos transaccionales, utilizado en el análisis de reglas de asociación."),
                                        tags$p("Proceso del Algoritmo FP-Growth"),
                                        tags$p("1.Construcción del Árbol FP:",
                                              tagList(
                                                 tags$ul(
                                                   tags$li("Escaneo Inicial: Se realiza un primer escaneo de la base de datos para contar la frecuencia de cada ítem. Luego, se eliminan los ítems que no alcanzan un umbral mínimo de soporte (es decir, los ítems infrecuentes)"),
                                                   tags$li("Ordenamiento de Ítems: Los ítems frecuentes se ordenan de acuerdo a su frecuencia, de mayor a menor."),
                                                   tags$p("Construcción del Árbol: Se crea un árbol (FP-tree) en el que cada nodo representa un ítem frecuente. Cada transacción se inserta en el árbol siguiendo el orden de los ítems frecuentes. Si un ítem ya existe en el árbol, se incrementa el conteo del nodo correspondiente; si no, se crea un nuevo nodo.")))),
                                        
                                        tags$p("2.Extracción de Conjuntos Frecuentes:",
                                               tagList(
                                                 tags$ul(
                                                   tags$li("Recorrido del Árbol: Para encontrar los conjuntos de ítems frecuentes, se realiza un recorrido del árbol comenzando desde los nodos hoja (los ítems más frecuentes)."),
                                                   tags$li("Generación de Condicionales: Para cada ítem, se generan los conjuntos de ítems condicionales, que son las transacciones que contienen ese ítem. Se construye un árbol condicional (Conditional FP-tree) para cada ítem.")))),
                                        
                                        img(src = 'FPG.jpg', style = "width:60%;height: auto; margin-bottom: 20px;")
                               ),
                     #Cuarto subpanel 
                     tabPanel("Ventajas de (Frequent Pattern Growth)", 
                              tags$br(), 
                              tags$p("1.Eficiencia"),
                              tags$p("A diferencia del algoritmo Apriori, que genera muchos conjuntos de ítems y requiere múltiples escaneos de la base de datos, FP-Growth solo necesita dos escaneos, lo que lo hace mucho más eficiente."),
                              tags$p("2.No genera conjuntos de Items Infrecuentes:"),
                              tags$p("Al construir el FP-tree, solo se consideran los ítems que cumplen con el umbral de soporte, lo que reduce significativamente la cantidad de datos a procesar.")
                                  
                    
                              
                                        
                      )
                    )
                )
             )),
                    
           
      
      # 4to tabItem: RMD
      tabItem(tabName='rmarkdown',
              titlePanel('RMD'),
              fluidRow(
                box(title='Rmd', width=12, status='warning',
                    uiOutput('rmd_output'),
                    downloadButton('download_rmd', 'Descargar Rmd')
                )
              )
      ),
      
      # 5to tabItem: Dataset
      tabItem(tabName = 'dataset',
              titlePanel('Datos'),
              fluidRow(
                box(
                  title = 'Descripción de los Datos',
                  width = 12,
                  status = 'warning',
                  tags$h5("Este conjunto de datos contiene 6 variables y 20507 observaciones sobre diferentes transacciones..."),
                  tags$p(tags$b("Nombre de columnas:")),
                  tags$ul(
                    tags$br(),
                    tags$li(tags$b("Transaction:"), " Numero de transacción"),
                    tags$li(tags$b("Item:"), " Producto comprado"),
                    tags$li(tags$b("Fecha:"), " Fecha de la compra"),
                    tags$li(tags$b("Hora:"), " Hora de la compra"),
                    tags$li(tags$b("Period_day:"), "Rango del dia de la compra"),
                    tags$li(tags$b("Weekday_weekend:"), " Rango de la compra: dia de semana / fin de semana"),
                    tags$br(),
                  ),

                
                box(title = 'Tabla de Datos', width = 12, status = 'warning', DTOutput('data_table')),
                box(title = 'Forma de los Datos', width = 6, status = 'warning', verbatimTextOutput('data_str'),class="box_custom"),
                box(title = 'Estadísticas Descriptivas', width = 6 ,status = 'warning', verbatimTextOutput('data_summary'),class="box_custom")
              )
      )),
      
      
      # 6to tabItem: EDA
      tabItem(tabName='eda',
              titlePanel('Análisis exploratorio de datos'),
              fluidRow(
                box(title = "Variables finales", width =12, status = 'warning',height="500px",
                    DTOutput("final_datatable")),
                
                tags$br(),
                box(title="Frecuencia de las Variables",
                    width=6,
                    status='warning',
                    solidHeader=TRUE,
                    selectInput(
                      "selected_freq_var",
                       "Seleccione una variable:", 
                      choices= c("Articulo","Periodo_del_dia","DiadeSem_FindeSem","Hora","Dia_Semana","Mes")),
                      plotlyOutput("frequency_plot",height="500px")
                      ),
                box(title="Cantidad de Transacciones Apiladas por Día de la Semana"
                    ,width = 6,
                    status='warning',
                    solidHeader = TRUE,plotlyOutput("apiladas",height="580px"))
           
        ),
           fluidRow(
             box(title="Top 10 Items segun el Periodo del Día:",
                 width=6,
                 selectInput("period_day", "Selecciona el Periodo del Día:", 
                             choices = unique(data$period_day), 
                             selected = unique(data$period_day)[1]),  # Selecciona el primero por defecto
                 tags$br(), 
                 plotlyOutput("column_plot",height = "500px")
             
                 ),
             box(title = "Heatmap de Ventas por Día de la Semana y Hora",
                 width = 6,
                 selectInput("periodo_del_dia", "Selecciona el Periodo del Día:", 
                             choices = unique(datos_finales$Periodo_del_dia), 
                             selected = unique(datos_finales$Periodo_del_dia)[1]),  # Selecciona el primero por defecto
                 selectInput("mes", "Selecciona el Mes:", 
                             choices = unique(datos_finales$Mes), 
                             selected = unique(datos_finales$Mes)[1]),  # Selecciona el primer mes por defecto
                 tags$br(), 
                 plotlyOutput("heatmap_plot",height = "420px")  # Gráfico interactivo
             )
           ),
        fluidRow(
          box(title="Distribución de los 10 Artículos Más Vendidos:",
              width=6,
              tags$br(), 
              plotlyOutput("dona_chart",height = "500px")
              
          )
        )),
      
      # 7to tabItem: Regla asociacion
      
      tabItem(tabName = "reglas",
              titlePanel('Algoritmos'),
              fluidRow(
                box(width = 12,
                    tabsetPanel(
                      tabPanel("Algoritmo Apriori", 
                               tags$br(),
                               h4("Análisis de Reglas de Asociación - Apriori"),
                               fluidRow(
                                 column(6, sliderInput("supp", "Soporte:", min = 0.01, max = 0.05, value = 0.01, step = 0.01)),  
                                 column(6, sliderInput("conf", "Confianza:", min = 0.3, max = 0.7, value = 0.5, step = 0.05)),
                                 column(6, sliderInput("lift", "Lift Mínimo:", min = 1, max = 5, value = 1, step = 0.1))  # Agregado aquí
                               ),
                               tags$br(),
                               DTOutput("tabla_rules"),
                               fluidRow(
                                 column(width = 12,
                                        box(title = "Gráfico de Asociación", width = NULL, status = "warning",      
                                            visNetworkOutput("graph_rules"))
                                 )
                               )
                      ),
                      
                      tabPanel("Algoritmo FP-Growth", 
                               tags$br(),
                               h4("Análisis de Itemsets Frecuentes con FP-Growth"),
                               fluidRow(
                                 column(6, sliderInput("supp1", "Soporte mínimo:", min = 0.01, max = 0.6, value = 0.01, step = 0.01)),

                               ),
                               tags$br(),
                               DTOutput("itemsets_table"),
                               fluidRow(
                                 column(width = 12,
                                        box(title = "Gráfico de Asociación", width = NULL, status = "warning",      
                                            visNetworkOutput("graph_rules1")))
                              
                 )
                    
           )
              
      )))),
      
      tabItem(tabName = "simulacion",
              titlePanel('Simulación para campañas de Marketing'),
              sidebarLayout(
                sidebarPanel(
                  selectInput("producto", "Seleccione Producto:", choices = NULL),  # UI dinámico para productos
                  actionButton("calcular", "Calcular Promoción"),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  valueBoxOutput("descuento_valuebox", width = 12)  # ValueBox para el descuento
                ),
                mainPanel(
                  uiOutput(("resultado_promocion")
                )
              )
      )
      
      
      
  )
)))



server <- function(input, output,session) {
  
  ###### DATASET
  # Generar el archivo HTML del rmd
  observe({
    rmd_file <- "Practica_Asociacion.Rmd"
    html_file <- "www/report.html"
    # Renderiza el archivo HTML si no existe
    if (!file.exists(html_file)) {
      rmarkdown::render(rmd_file, output_format = "html_document", output_file = html_file)
    }
  })
  
  # Mostrar el archivo HTML en un iframe
  output$rmd_output <- renderUI({
    if (file.exists("www/report.html")) {
      tags$iframe(src = "report.html", width = "100%", height = "600px")
    } else {
      tagList(tags$p("El archivo HTML no esta disponible."))
    }
  })
  
  # Descarg rmd
  output$download_rmd <- downloadHandler(
    filename = function() {
      paste("Practica_Asociacion", ".Rmd", sep = "")
    },
    content = function(file) {
      file.copy("Practica_Asociacion.Rmd", file)
    }
  )
  
  output$data_str <- renderPrint({str(data)})
  output$data_summary <- renderPrint({summary(data)})
  output$data_table <- renderDT({
    datatable(data, options = list(
      scrollX = TRUE, pageLength = 5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')  })
    
  output$final_datatable<-renderDT({
    datatable(final_dataset, options = list(
      scrollX = TRUE, pageLength =5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')
  })
  
  output$data_nulos <- renderTable({
    data_nulos  
  }, rownames = TRUE)  


  

  ###### EDA
  
  # Gráfico de frecuencia según la variable seleccionada
  output$frequency_plot <- renderPlotly({
    selected_var <- input$selected_freq_var 

    # Generar el gráfico correcto basado en la selección del usuario
    if (selected_var == "Articulo") {
      plot <- barItem 
    } else if (selected_var == "Periodo_del_dia") {
      plot <- barperioday
    } else if (selected_var == "DiadeSem_FindeSem") {
      plot <- barweekday
    
    } else if (selected_var=="Hora"){
      plot <- barhora
      
    } else if (selected_var=="Dia_Semana"){
      plot <- barday
    } else if (selected_var=="Mes"){
    plot <- barmes
  }
  
    
    print(plot)
    
      plot
  })
  


  output$apiladas<-renderPlotly(
    apiladas
  )
 

  
  output$column_plot <- renderPlotly({
    generarColumnPlot(input$period_day, data, color)
  })

  output$heatmap_plot <- renderPlotly({
    generarHeatmap(datos_finales, input$periodo_del_dia, input$mes)
  })
  
  output$dona_chart <- renderPlotly({
    dona_chart
  })
  ###### Regla de Asociacion
  
  #Algoritmo APRIORI
  
  output$tabla_rules <- renderDT({
    # Generar reglas de asociación con los parámetros dinámicos de soporte y confianza
    rules_apriori <- apriori(transacciones_arules, parameter = list(supp = input$supp, conf = input$conf, target = "rules"))
    
    # Filtrar por lift
    rules_apriori <- subset(rules_apriori, lift > input$lift)
    
    # Ordenar las reglas por lift de forma descendente
    rules_apriori <- sort(rules_apriori, by = "lift", decreasing = TRUE)
    
    
    # Convertir las reglas a un formato legible
    rules_df <- as(rules_apriori, "data.frame")
    
    # Redondear las columnas numéricas a dos decimales
    rules_df$lift <- round(rules_df$lift, 2)
    rules_df$support <- round(rules_df$support, 2)
    rules_df$confidence <- round(rules_df$confidence, 2)
    rules_df$coverage <- round(rules_df$coverage, 2)
    
    # Mostrar la tabla usando DT
    datatable(rules_df)
  })
   
  output$graph_rules <- renderVisNetwork({
    plot(rules_apriori, method = "graph", engine = "htmlwidget")
  })
  
  
  #Algoritmo FP
  
  # Mostrar los itemsets frecuentes en una tabla
  output$itemsets_table <- renderDT({
    
    itemsets_frecuentes <- apriori(transacciones_fp, 
                                   parameter = list(supp = input$supp1, target = "frequent itemsets"))
    
    # Ordenar las reglas por lift de forma descendente
    itemsets_frecuentes <- sort(itemsets_frecuentes, by = "supp", decreasing = TRUE)
    
    # Convertir los resultados a un dataframe para mostrarlos en la tabla
    itemsets_df <- as(itemsets_frecuentes, "data.frame")
    
    
    # Redondear la columna 'supp' a 2 decimales
    itemsets_df$support <- round(itemsets_df$support, 2)
    
    # Mostrar la tabla con la opción de paginación
    datatable(itemsets_df, options = list(pageLength = 10))
  })
  
  output$graph_rules1 <- renderVisNetwork({
    plot(itemsets_frecuentes, method = "graph", engine = "htmlwidget")
  })
  
#####SIMULACION 
  # Crear una función para actualizar la lista de productos basada en reglas
  update_producto_choices <- function() {
    lhs_items <- labels(lhs(top_10_rules))
    rhs_items <- labels(rhs(top_10_rules))
    
    # Convertir a un vector de caracteres
    lhs_items <- as.character(lhs_items)
    rhs_items <- as.character(rhs_items)
    
    # Unir los productos de lhs y rhs
    productos <- unique(c(lhs_items, rhs_items))
    
    # Filtrar para evitar vacíos
    productos <- productos[productos != ""]
    
    # Actualizar el selectInput con los productos disponibles
    updateSelectInput(session, "producto", choices = productos)
  }
  
  # Llamar a la función para actualizar productos al iniciar la aplicación
  observe({
    update_producto_choices()
  })
  
  # Observador para calcular promociones
  observeEvent(input$calcular, {
    # Asegurarse de que lhs y rhs estén correctamente en formato de texto
    top_10_rules_df$lhs <- as.character(labels(lhs(top_10_rules)))
    top_10_rules_df$rhs <- as.character(labels(rhs(top_10_rules)))
    
    # Filtrar las reglas de asociación que incluyan el producto seleccionado
    reglas_filtradas <- top_10_rules_df %>%
      filter(grepl(input$producto, lhs, fixed = TRUE)) 
    
    # Comprobar si hay reglas aplicables
    if (nrow(reglas_filtradas) > 0) {
      recomendaciones <- paste0(
        "<h3>Si el cliente compra <strong>", input$producto, "</strong>, entonces:</h3>"
      )
      # Crear un mensaje de recomendaciones basado en las reglas aplicables
      recomendaciones <- paste0(recomendaciones, 
                                "<p><strong>Estas son las métricas asociadas:</strong></p>")
      
      # Iterar sobre las reglas filtradas para mostrar las recomendaciones
      for (i in 1:nrow(reglas_filtradas)) {
        recomendaciones <- paste0(
          recomendaciones, 
          "<br><ul><li><strong>Support: </strong>", round(reglas_filtradas$support[i] * 100, 2), "%", 
          "</li><li><strong>Confidence: </strong>", round(reglas_filtradas$confidence[i] * 100, 2), "%", 
          "</li><li><strong>Lift: </strong>", round(reglas_filtradas$lift[i], 4), 
          "</li></ul>"
        )
      }
    } else {
      recomendaciones <- "<h3>No se encontraron recomendaciones para este producto.</h3>"
    }
    
    producto_elegido <- input$producto
    
    # Asignar descuento según el producto elegido con rango dinámico
    descuento <- floor(runif(1, min = 5, max = 20))
    
    # Mostrar el valueBox con el descuento y barra de progreso
    output$descuento_valuebox <- renderValueBox({
      valueBox(
        paste0(descuento, "%"),  # Mostrar el valor del descuento
        subtitle = paste("Descuento en", producto_recomendado),
        icon = icon("tags"),
        color = ifelse(descuento >= 12, "light-blue", "orange")
      )
    })
    
    output$barra_descuento <- renderUI({
      tags$div(
        class = "progress-bar",
        style = paste0(
          "width: ", descuento, "%; background-color: ",
          ifelse(descuento > 15, "#00cc66", "#ff9933"), ";"
        ),
        paste0(descuento, "%")
      )
    })
    
    # Determinar la imagen según el producto elegido
    imagen_prod <- switch(
      producto_elegido,
      "{Toast}" = "Toast.png",
      "{Medialuna}" = "Medialuna.png",
      "{Pastry}" = "Pastry.png",
      "{Cake}" = "Cake.png",
      "{Juice}" = "Juice.png",
      "{Cookies}" = "Cookie.png",
      "{Spanish Brunch}" = "Brunch.png",
      "{Alfajores}" = "Alfajores.png",
      "{Scone}" = "Scone.png",
      "{Hot chocolate}" = "Hot_chocolate.png",
      "{Sandwich}" = "Sandwich.png",
      "default_image.png"  # Imagen por defecto
    )
    
    producto_recomendado <- gsub("\\{|\\}", "", reglas_filtradas$rhs[1])  # Primera recomendación
    
    # Determinar la imagen según el producto recomendado
    imagen_path <- switch(
      producto_recomendado,
      "Coffee" = "Cafe.jpeg",
      "Medialuna" = "Medialuna.png",
      "default_image.png"  # Imagen por defecto
    )
    
    # Mostrar las imágenes del producto elegido y el recomendado, junto con recomendaciones
    output$resultado_promocion <- renderUI({
      tagList(
        HTML(recomendaciones),  # Mostrar las recomendaciones en HTML
        tags$div(
          style = "display: flex; justify-content: center; gap: 20px;",  # Flexbox para alinear en fila
          tags$img(src = imagen_prod, height = "300px", width = "300px"),  # Imagen del producto
          tags$img(src = imagen_path, height = "300px", width = "300px")   # Imagen del recomendado
        ),
        # Barra de progreso del descuento
        tags$div(
          class = "progress",
          tags$div(id = "barra_descuento")
        )
      )
    })
  })
  
  

  
  }

shinyApp(ui, server)


