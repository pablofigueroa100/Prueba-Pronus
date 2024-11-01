#-------------------------------------------------------------------------------
#-------------------------C�digo prueba t�cnica Pronus--------------------------
#------------------------Pablo Andr�s Figueroa Chamorro-------------------------
#-------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)

rm(list = ls())

#-----------------------------Peparaci�n de la base-----------------------------

#Cargar las bases de datos, seleccionar los datos solicitados y a�adir una
#columna titulada ref_doc, cuyo n�mero se seleccion� para facilitar el cambio de
#formato a Date

enero <- read_excel("datos_prueba/0122.xlsx", 
                    sheet = "TarjetasXFranquicia", skip = 7) %>% 
                    select(c(3,24,25,26,27)) %>% slice_head(n=-3) %>%
                    mutate(ref_doc=18993)
febrero <- read_excel("datos_prueba/0222.xlsx", 
                      sheet = "TarjetasXFranquicia", skip = 7) %>% 
                      select(c(3,24,25,26,27)) %>% slice_head(n=-3) %>%
                      mutate(ref_doc=19024)
marzo <- read_excel("datos_prueba/0322.xlsx", 
                    sheet = "TarjetasXFranquicia", skip = 7) %>% 
                    select(c(3,24,25,26,27)) %>% slice_head(n=-3) %>%
                    mutate(ref_doc=19052)
abril <- read_excel("datos_prueba/0422.xlsx", 
                    sheet = "TarjetasXFranquicia", skip = 7) %>% 
                    select(c(3,24,25,26,27)) %>% slice_head(n=-3) %>%
                    mutate(ref_doc=19083)

#?read_excel
#?slice_head
#?mutate

#Unir las bases y visualizar

data <- bind_rows(enero, febrero, marzo, abril)
data

#--------------------------Columnas num�ricas y fecha---------------------------

#Cambiar nombre de las columnas y formatos (solo fue necesario el de la fecha,
#los otros ya estaban como dbl) y viaulizar

data1 <- data %>% rename(Entidad=...1,vigente_corte=`Vigentes  a la fecha de corte`
                        ,vigente_mes=`Vigentes durante el mes`,
                        canceladas=Canceladas, bloqueadas=`Bloqueadas  temporalmente`,
                        fecha=ref_doc) %>% mutate(fecha=as.Date(fecha))
data1
#?rename

#------------------------------An�lisis de datos--------------------------------

#Agrupar la data por fecha y mostrar sumatoria por canceladas

data_grafica <- data1 %>% group_by(fecha) %>% summarise(sum = sum(canceladas))
data_grafica

#Editar gr�fica seg�n requerimientos

grafica <-  ggplot(data_grafica, aes(x = fecha, y = sum)) +
  geom_line(size=1) + 
  labs(title = "Evoluci�n Tarjetas Canceladas", subtitle = "enero a abril del 2022", 
       y = "N�mero de tarjetas", x = "Fecha", caption = "Datos tomados de la SFC") +
  theme_minimal() +
  theme(plot.title = element_text(color =  "#A21824", hjust = 0.5), 
        plot.subtitle = element_text(color =  "#FE4902", hjust = 0.5),
        axis.title.x = element_text(color =  "#595959"),
        axis.title.y = element_text(color =  "#595959"),
        plot.caption = element_text(color =  "#595959"),
        axis.line = element_line(color = "#595959", size = 1))

windows()  
grafica

#------------------------------Preguntas finales--------------------------------

#1) �La gr�fica presentada arriba es congruente con respecto al t�tulo,
#    subtitulo, ejes, etc? �Por qu� no? �Qu� le a�adir�a?

#En t�rminos generales, la congruencia de la gr�fica con el t�tulo, subt�tulo,
#eje, etc, es aceptable. Sin embargo, respecto a los ejes, podr�a modificarse las
#etiquetas para que la gr�fica sea m�s clara, concretamente, podr�a utilizarse
#"N�mero de Tarjetas Canceladas" en el eje Y y "Mes" en el eje X. Adem�s, con
#el objetivo de facilitar la lectura de la gr�fica podr�a modificarse la escala
#del eje Y y presentar los datos en miles. 

#2) �Cu�l es crecimiento mensual promedio de las tarjetas canceladas?

#Crecimiento promedio absoluto = 32128.67
mean(diff(data_grafica$sum))

# Tasas de crecimiento promedio = 14.28849%
mean(diff(data_grafica$sum) / head(data_grafica$sum, -1))


#3) �Cu�l fue el Establecimiento de Cr�dito que m�s tarjetas cancel� durante
#    todo el horizonte de tiempo? = Tuya

data2 <- data1 %>% group_by(Entidad) %>% summarise(sum = sum(canceladas))
data2$Entidad[which.max(data2$sum)]

#?which.max

#4) El cliente le pregunta �Cu�l es la raz�n de est� tendencia? Detalle los
#   pasos a seguir de su metodolog�a, entre m�s robusta sea, mejor.

#En primer lugar, para responder a la pregunta planteada es necesario aumentar
#la cantidad de periodos observados, puesto que revisando �nicamente 4 meses es
#realmente complejo llegar a una soluci�n m�s precisa y robusta. Para esto
#puede consultarse la fuente de la que originalmente se obtuvo los datos (SFC).

#Ahora bien, una vez ya se tiene determinada la variable end�gena, es necesario
#determinar las ex�genas. Un buen inicio es revisar variables macroecon�micas,
# como puede ser la tasa desempleo, ya que Un aumento en el desempleo puede
#reducir la capacidad de las personas para pagar sus deudas y llevarlos a cancelar
#ciertos productos financieros como las tarjetas. La tasa de inter�s de colocaci�n
#es una opci�n interesante, ya que estas, al ser muy altas, pueden desincentivar
#el uso de tarjetas de cr�dito. Ingreso y/o consumo promedio, puesto que pueden
#modificar las necesidades en t�rminos de pagos de la poblaci�n y, por ende,
#inducir a un aumento en la cancelaci�n de tarjetas en caso de ser muy bajo.

#Por otro lado, tambi�n es necesario a�adir variables que modelen factores del
#sistema financiero y gubernamental. Por ejemplo, podr�a incluirse, en forma de
#variable dicot�mica, la introducci�n de nuevos productos financieros al mercado,
#como dep�sitos de bajo monto, que cubran necesidades de pago similares a las de
#las tarjetas. Adem�s, tambi�n podr�a analizarse �ndices de morosidad, puesto que
#puede inducir a las personas a cancelar productos por su incapacidad de pagarlos.
#Por otro lado, puede incluirse variables dicot�micas que modelen la introducci�n
#de cambios regulatorios o est�mulos eocon�micos relacionados con el uso de
#tarjetas.

#Finalmente, tambi�n podr�a incluirse variables dicot�micas para determinados
#periodos del a�o para analizar posibles fluctuaciones causadas por determinados
#eventos, como pueden ser navidad, vacaciones, entre otros.

#Una vez recopilados los datos, es necesario seleccionar un modelo econom�trico
#que permita localizar las relaciones de causalidad que solicita el cliente. Es
#por ello que modelos ARIMA o de machine learning pueden no ser los ideales, ya
#que estos no est�n dise�ados para buscar causalidad. Una buena opci�n, dada su
#congruencia con los objetivos planteados y flexibilidad, puede ser un modelo de
#regresi�n din�mica con variables ex�genas, como el ARDL.

#Para utilizar el modelo ARDL es necesario cargar el paquete "ARDL". el modelo
#se crear�a mediante los siguientes c�digos:

#modelo1 <- auto_ardl(Y ~ X, data = data, max_order = c(5, 5), selection = "AIC")
#modelo2 <- auto_ardl(Y ~ X, data = data, max_order = c(5, 5), selection = "BIC")

#Lo ideal en este punto es que la cantidad de regresores de la variable end�gena
#y de las variables ex�genas coincidan en ambos modelos, sin embargo, en caso de
#que no lo hagan, lo m�s conveniente ser�a dar prioridad al modelo seleccionado
#y estimado por el criterio BIC, ya que este favorece m�s la parsimonia que el AIC.

#Una vez especificado el modelo, puede verse los resultados a trav�s de:

#summary(modelo2)

#y la cointegraci�n a largo plazo con:

#coint_eq(modelo2)

#Ahora bien, para validar los resultados del modelo, es necesario aplicar ciertas
#pruebas estad�sticas. Para ello es necesario utilizar los paquetes "lmtest" y
#"car". Para corroborar la no presencia de cambios estructurales, se realiza el
#test CUSUM:

#cusum <- cusum(modelo2)
#plot(cusum)

#En caso de que la l�nea salga de los l�mites establecidos por la prueba, entonces
# puede ser necesario incluir una dummy adicional que modele el cambio
#estructural. Ahora bien, Para la autocorrelaci�n de errores se puede emplear la
#prueba de Breusch-Godfrey o la prueba de Durbin-Watson:

#bgtest(modelo2)
#dwtest(modelo2)

#En este caso un p-valor bajo puede indicar presencia de autocorrelaci�n en los
#errores, por lo que puede ser necesario ajustar la especificaci�n del modelo.
#Por otro lado, la heteroceasticidad puede ser probada con el test de
#Breusch-Pagan:

#bptest(modelo2)

#En caso de detectar heteroceasticidad a trav�s de un p-valor bajo, es necesario
#estimar el modelo a trav�s de GLS o m�xima verosimilitud. A continuaci�n, se
#aplica la prueba VIF:

#vif(modelo2)

#En caso de que la prueba arroje un n�mero alto, puede ser mayor o igual a 10, es
#necesario revisar la especificaci�n del modelo y eliminar alguna variable o
#integrarla con otra.

#Finalmente, despu�s de implementar los ajustes sugeridos por los resultados de
#las pruebas estad�sticas, se puede concluir que las estimaciones del modelo son
#apropiadas para explicar la tendencia observada en la gr�fica inicial en lo que
# a cancelaci�n de tarjetas respecta.