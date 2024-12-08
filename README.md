# Reglas de Asociación
Proyecto Reglas Asociación:
### 1. Carga y Exploración de Datos
Se cargó un archivo CSV de ventas de la panadería (bakery_sales_revised.csv).
Se exploró la estructura del dataset usando str() y summary(), y se generaron tablas interactivas con datatable().
### 2. Procesamiento de Fechas y Horas
Se separó la columna de fecha y hora (date_time) en dos columnas: Fecha y Hora.
Se convirtió la columna Fecha al formato de fecha POSIXct y se extrajeron el mes y el día.
Se ajustó la columna Hora para representarla en intervalos de 1 hora (ej., 1-2, 2-3, etc.).
Se renombraron los días de la semana usando la función wday().

### 3. Generación de Gráficos
Se crearon varios gráficos para visualizar la frecuencia de las categorías:
Gráficos de barras para las columnas Articulo, Periodo_del_dia, Dia_Semana, Mes, y Hora.
Gráfico apilado para analizar transacciones según el día de la semana y el período del día.
Gráfico de columnas que muestra las transacciones por artículo en un período específico del día.
Mapa de calor (heatmap) para visualizar las ventas por día de la semana y período del día.
Gráfico de dona que muestra los 10 artículos más vendidos.
### 4. Análisis de Reglas de Asociación con Apriori y FP-Growth
Se aplicó el algoritmo Apriori para generar reglas de asociación entre productos, con un soporte mínimo de 0.01 y una confianza mínima de 0.5.
Las reglas generadas se ordenaron por "lift" y se mostraron las 10 mejores.
Se utilizó el algoritmo FP-Growth para generar itemsets frecuentes y reglas de asociación, similar al Apriori.
Este código proporciona un análisis detallado de las ventas y patrones de compra, así como herramientas para realizar análisis de asociación de productos. Se generaron gráficos interactivos y mapas de calor para una visualización más clara de los datos, utilizando librerías como ggplot2, plotly, y arules.

### 5. Simulación de Promociones y Recomendaciones:

La aplicación permite al usuario seleccionar un producto, y, basado en reglas de asociación, genera recomendaciones de productos adicionales con métricas de soporte, confianza y lift.
Se asigna un descuento aleatorio entre 5% y 20%, que se muestra en un valueBox y una barra de progreso. El color de la barra varía según el valor del descuento.
Se muestran imágenes del producto seleccionado y recomendado. Las imágenes y las recomendaciones se ajustan dinámicamente según el producto elegido y las reglas de asociación correspondientes.


#LINK A LA SHINY APP https://rociomartinez.shinyapps.io/reglas_de_asociacion/
