# Mon Dec 13 17:30:34 2021 ------------------------------
# Config workspace --------------------------------------------------------
  install.packages("semTools")
  library(semTools)
  rm()

# Open database -----------------------------------------------------------
  DATABASE <- read.spss("magcont.sav") #se usa el paquete "foreign" para abrir archivos de spss, "ggplot2", "tidyverse" y "car"
  DATABASE <- as.data.frame(DATABASE)
  View(DATABASE)
  colnames(DATABASE)

# Analysis ----------------------------------------------------------------
  #Crosstabs
    str(DATABASE$sexo) #configurar las variables categoricas para las tablas de contingencia

    table(DATABASE$sexo, DATABASE$eciv)
    table(DATABASE$sexo, DATABASE$reli)
    table(DATABASE$sexo, DATABASE$opol)
    table(DATABASE$sexo, DATABASE$educ)
    table(DATABASE$sexo, DATABASE$eduh)
    table(DATABASE$sexo, DATABASE$etni)

    DATABASE$naci <- recode(DATABASE$naci, '1962:1980 = "Entre 59 y 41" ; 1981:1999 = "Entre 40 y 20" ')
    #recodificacion año de nacimiento, se utilizan paquetes "tidyverse" y "car"
    table(DATABASE$sexo, DATABASE$naci)

  #Graphs
    #grafico de barras solo para estado civil
    ggplot(data = DATABASE,
           mapping = aes(x = factor(eciv))) +
           geom_bar(fill="Purple")

    #se genera como df para agilizar el proceso
    Graph1 <- ggplot(data = DATABASE,
           mapping = aes(x = factor(eciv), fill = factor(sexo)))

    #data + grafico apilado]
    Graph1 + geom_bar(position = 'stack', stat = 'count') + coord_flip() + theme_dark() + scale_fill_manual(values=c("Black","Red"))

    #data + grafico agrupado
    Graph1 + geom_bar(position = 'dodge', stat = 'count') + coord_flip() + theme_minimal() + scale_fill_manual(values = c("Green", "pink"))

    #data + grafico apilado al 100%
    Graph1 + geom_bar(position = 'fill', stat = 'count') + coord_flip()+ theme_linedraw() + scale_fill_manual(values =c("Blue", "Yellow"))

# Psychometric Analysis ---------------------------------------------------
  #Analisis factorial confirmatorio de SDSS

  onef <- 'sdss =~ sdss1 + sdss2 + sdss3 + sdss4 + sdss5'

  CFA1 <- cfa(onef,orthogonal=TRUE, data = DATABASE, estimator="WLSMV", ordered =names(DATABASE))
  summary(CFA1, fit.measures=TRUE)

  #sobre los resultados:
    #La prueba de chi-cuadrado da una significancia estadistica de .000, con 32,7 grados de libertad
    #Arroja un CFI de .785, un TLI de .57 (no alcanzan a ser mayores a .8 o .9) y un RMSEA de .254 (mayor a .1), por lo que no puede decirse que sea un buen modelo
