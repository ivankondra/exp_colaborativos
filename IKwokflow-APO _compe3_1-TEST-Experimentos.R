#Sys.sleep(3200) 
# limpio la memoria
Sys.time()
rm(list=ls(all.names=TRUE)) # remove all objects
gc(full=TRUE, verbose=FALSE) # garbage collection

PARAM <- list()
PARAM$experimento <- "apo-001"
PARAM$semilla_primigenia <- 102191

setwd("/content/buckets/b1/test_experimentos3")
experimento_folder <- PARAM$experimento
dir.create(experimento_folder, showWarnings=FALSE)
setwd( paste0("/content/buckets/b1/test_experimentos3/", experimento_folder ))

Sys.time()
require( "data.table" )

# leo el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_02_crudo.csv.gz" )
dataset2 <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz" )


dataset <- rbind (dataset, dataset2)
rm(dataset2)

library(data.table)

setDT(dataset)

cols_binarias <- names(dataset)[
  sapply(dataset, function(x) all(x %in% c(0, 1), na.rm = TRUE))
]

# Obtener lista de columnas numéricas (excepto numero_de_cliente y foto_mes)
cols_numeric <- setdiff(
  names(dataset),
  c("numero_de_cliente", "foto_mes", "active_quarter", 'internet', cols_binarias)
)

# Detectar columnas que son todo cero por mes
cols_cero_por_mes <- dataset[, lapply(.SD, function(x) all(x == 0)), 
                             by = foto_mes, 
                             .SDcols = cols_numeric]

problemas <- melt(cols_cero_por_mes, id.vars = "foto_mes")
problemas <- problemas[value == TRUE]

mes_anterior <- function(fm) {
  anio <- fm %/% 100
  mes  <- fm %% 100
  if (mes == 1) {
    return((anio - 1)*100 + 12)
  }
  return(anio*100 + mes - 1)
}

mes_siguiente <- function(fm) {
  anio <- fm %/% 100
  mes  <- fm %% 100
  if (mes == 12) {
    return((anio + 1)*100 + 1)
  }
  return(anio*100 + mes + 1)
}


# Asegurar que las columnas sean texto
problemas$variable <- as.character(problemas$variable)

for (i in seq_len(nrow(problemas))) {
    
    fm  <- problemas$foto_mes[i]
    col <- problemas$variable[i]

    # Validación
    if (!(col %in% names(dataset))) {
        cat("Columna", col, "no existe en dataset — se salta.\n")
        next
    }

    fm_prev <- mes_anterior(fm)
    fm_next <- mes_siguiente(fm)

    # ----- PREV -----
    prev_vals <- NULL
    if (fm_prev %in% dataset$foto_mes) {
        prev_vals <- dataset[foto_mes == fm_prev,
                             .(numero_de_cliente, prev = get(col))]
    }

    # ----- NEXT -----
    next_vals <- NULL
    if (fm_next %in% dataset$foto_mes) {
        next_vals <- dataset[foto_mes == fm_next,
                             .(numero_de_cliente, next_val = get(col))]
    }

    # Si no hay ni prev ni next, no se puede imputar
    if (is.null(prev_vals) && is.null(next_vals)) {
        cat("No existen meses prev/next para", col, "en", fm, "\n")
        next
    }

    # ----- ARMAMOS LA TABLA DEL MES ACTUAL -----
    todo <- dataset[foto_mes == fm,
                    .(numero_de_cliente, actual = get(col))]

    # Hacemos join con prev y next si existen
    if (!is.null(prev_vals)) todo <- prev_vals[todo, on="numero_de_cliente"]
    if (!is.null(next_vals)) todo <- next_vals[todo, on="numero_de_cliente"]

    # ----- IMPUTACIÓN -----
    if (all(c("prev", "next_val") %in% names(todo))) {
        todo[, imputado := (prev + next_val) / 2]
    } else {
        # Si falta uno de los dos, NO imputamos
        todo[, imputado := NA_real_]
    }

    # ----- REEMPLAZO SOLO DONDE EL VALOR ES 0 -----
    dataset[
        foto_mes == fm & get(col) == 0,
        (col) := todo[match(numero_de_cliente, todo$numero_de_cliente), imputado]
    ]

    #cat("Imputado:", col, "para el mes", fm, "\n")
}

# calculo el periodo0 consecutivo
dsimple <- dataset[, list(
  "pos" = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 )
]


# ordeno
setorder( dsimple, numero_de_cliente, periodo0 )

# calculo topes
periodo_ultimo <- dsimple[, max(periodo0) ]
periodo_anteultimo <- periodo_ultimo - 1


# calculo los leads de orden 1 y 2
dsimple[, c("periodo1", "periodo2") :=
  shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente
]

# assign most common class values = "CONTINUA"
dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

# calculo BAJA+1
dsimple[ periodo0 < periodo_ultimo &
  ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
  clase_ternaria := "BAJA+1"
]

# calculo BAJA+2
dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
  & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
  clase_ternaria := "BAJA+2"
]

# pego el resultado en el dataset original y grabo
setorder( dsimple, pos )
dataset[, clase_ternaria := dsimple$clase_ternaria ]

rm(dsimple)
gc()
Sys.time()

setorder( dataset, foto_mes, clase_ternaria, numero_de_cliente)
dataset[, .N, list(foto_mes, clase_ternaria)]

# Salsa Magica para 202106
#dataset[, mprestamos_personales := NULL ]
#dataset[, cprestamos_personales := NULL ]

# el mes 1,2, ..12 , podria servir para detectar estacionalidad
dataset[, kmes := foto_mes %% 100]

# creo un ctr_quarter que tenga en cuenta cuando
# los clientes hace 3 menos meses que estan
# ya que seria injusto considerar las transacciones medidas en menor tiempo
dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5.0]
dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2.0]
dataset[cliente_antiguedad == 3, ctrx_quarter_normalizado := ctrx_quarter * 1.2]

# variable extraida de una tesis de maestria de Irlanda, se perdió el link
dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

Sys.time()

if( !require("Rcpp")) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
require("Rcpp")

# se calculan para los 6 meses previos el minimo, maximo y
#  tendencia calculada con cuadrados minimos
# la formula de calculo de la tendencia puede verse en
#  https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
# para la maxíma velocidad esta funcion esta escrita en lenguaje C,
# y no en la porqueria de R o Python

cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}")

# calcula la tendencia de las variables cols de los ultimos 6 meses
# la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
# La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE) {
  gc(verbose= FALSE)
  # Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion <- ventana

  last <- nrow(dataset)

  # creo el vector_desde que indica cada ventana
  # de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids <- dataset[ , numero_de_cliente ]

  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )

  vector_desde[1:ventana_regresion] <- 1

  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }

  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
        nueva_col[(0 * last + 1):(1 * last)]]
    }

    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
        nueva_col[(1 * last + 1):(2 * last)]]
    }

    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
        nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
        nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
        get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
        get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}

# Feature Engineering Historico
# Creacion de LAGs
setorder(dataset, numero_de_cliente, foto_mes)

# todo es lagueable, menos la primary key y la clase
cols_lagueables <- copy( setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
))

# https://rdrr.io/cran/data.table/man/shift.html

# lags de orden 1
dataset[,
  paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
  by= numero_de_cliente,
  .SDcols= cols_lagueables
]

# lags de orden 2
dataset[,
  paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
  by= numero_de_cliente,
  .SDcols= cols_lagueables
]

# agrego los delta lags
for (vcol in cols_lagueables)
{
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

Sys.time()

# parametros de Feature Engineering Historico de Tendencias
PARAM$FE_hist$Tendencias$run <- TRUE
PARAM$FE_hist$Tendencias$ventana <- 6
PARAM$FE_hist$Tendencias$tendencia <- TRUE
PARAM$FE_hist$Tendencias$minimo <- FALSE
PARAM$FE_hist$Tendencias$maximo <- FALSE
PARAM$FE_hist$Tendencias$promedio <- FALSE
PARAM$FE_hist$Tendencias$ratioavg <- FALSE
PARAM$FE_hist$Tendencias$ratiomax <- FALSE

# aqui se agregan las tendencias de los ultimos 6 meses

cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
setorder(dataset, numero_de_cliente, foto_mes)

if( PARAM$FE_hist$Tendencias$run) {
    TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = PARAM$FE_hist$Tendencias$ventana, # 6 meses de historia
    tendencia = PARAM$FE_hist$Tendencias$tendencia,
    minimo = PARAM$FE_hist$Tendencias$minimo,
    maximo = PARAM$FE_hist$Tendencias$maximo,
    promedio = PARAM$FE_hist$Tendencias$promedio,
    ratioavg = PARAM$FE_hist$Tendencias$ratioavg,
    ratiomax = PARAM$FE_hist$Tendencias$ratiomax
  )
}

ncol(dataset)
Sys.time()

ncol(dataset)
colnames(dataset)

# paso la clase a binaria que tome valores {0,1}  enteros
#  BAJA+1 y BAJA+2  son  1,   CONTINUA es 0
#  a partir de ahora ya NO puedo cortar  por prob(BAJA+2) > 1/40

dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)]

PARAM$trainingstrategy$testing <- c(202107)

PARAM$trainingstrategy$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105
)

PARAM$trainingstrategy$undersampling <- 0.05


PARAM$trainingstrategy$positivos <- c( "BAJA+1", "BAJA+2")

# los campos en los que se entrena
campos_buenos <- copy( setdiff(
    colnames(dataset), c("clase_ternaria","clase01","azar"))
)

# Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINIA"

set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]

dataset[  foto_mes %in%  PARAM$trainingstrategy$training &
  (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]


if( !require("lightgbm")) install.packages("lightgbm")
require("lightgbm")

dtrain <- lgb.Dataset(
  data= data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label= dataset[training == 1L, clase01],
  free_raw_data= TRUE
)

cat("filas", nrow(dtrain), "columnas", ncol(dtrain), "\n")
Sys.time()

# defino los datos de testing
dataset_test <- dataset[foto_mes %in% PARAM$trainingstrategy$testing]

# precalculo el campo de la ganancia
dataset_test[, gan := -20000.0 ]
dataset_test[ clase_ternaria=="BAJA+2", gan := 780000]

# precalculo la test_matrix
test_matrix <- data.matrix(dataset_test[, campos_buenos, with= FALSE])

cat("filas", nrow(dataset_test), "columnas", ncol(dataset_test), "\n")
Sys.time()

# paquetes necesarios para la Bayesian Optimization
if(!require("DiceKriging")) install.packages("DiceKriging")
require("DiceKriging")

if(!require("mlrMBO")) install.packages("mlrMBO")
require("mlrMBO")

# Especificacion de la cantidad de iteraciones de la Bayesian Optimization
# 50 es razonable
PARAM$hipeparametertuning$BO_iteraciones <- 25 # un 50 seria mas razonable

# parametros fijos del LightGBM
PARAM$lgbm$param_fijos <- list(
  objective= "binary",
  metric= "custom",
  first_metric_only= TRUE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  verbosity= -100,
  force_row_wise= TRUE, # para evitar warning
  seed= PARAM$semilla_primigenia,
  extra_trees = FALSE,

  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  max_bin= 31
)

# Notar que se recorren algunos hiperparametros en forma logaritmica
#   y que con forbidden se tiene en cuenta el juego que hay entre min_data_in_leaf y num_leaves

PARAM$hipeparametertuning$hs <- makeParamSet(
  makeNumericParam("num_iterations", lower= 0.0, upper= 11.1, trafo= function(x) as.integer( round(2^x)) ),
  makeNumericParam("learning_rate", lower= -8.0, upper= -1.0, trafo= function(x) 2^x ),
  makeNumericParam("feature_fraction", lower= 0.05, upper= 1.0 ),
  makeNumericParam("min_data_in_leaf", lower= 0.0, upper= log2(nrow(dtrain)/2), trafo= function(x) as.integer(round(2^x)) ),
  makeNumericParam("num_leaves", lower= 1.0, upper= 10.0, trafo= function(x) as.integer(round(2^x)) ),
  forbidden= quote( (2^min_data_in_leaf)*(2^num_leaves) > nrow(dtrain) )
)

PARAM$hipeparametertuning$ksemillerio <- 1L
PARAM$hipeparametertuning$repe <- 1L


if(!require("primes")) install.packages("primes")
require("primes")

primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
# me quedo con PARAM$semillerio  primos al azar
PARAM$BO$semillas <- sample(primos)[seq( PARAM$hipeparametertuning$ksemillerio * PARAM$hipeparametertuning$repe )]

cat( PARAM$BO$semillas)

if(!require("rlist")) install.packages("rlist")
require("rlist")

# logueo al archivo BO_log.txt
loguear  <- function( reg, arch=NA, verbose=TRUE )
{
  t0 <- Sys.time()
  archivo <- arch
  if( is.na(arch) ) archivo <- paste0( folder, substitute( reg), ext )


  if( !file.exists( archivo ) )
  {
    # Escribo los titulos
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  # escribo el registro
  linea  <- paste0( format(t0, "%Y%m%d.%H%M%S"),  "\t",     # la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  # grabo al archivo

  if( verbose )  cat( linea )   # imprimo por pantalla
}

# esto esta en una funcion para que el garbage collector lo libere
# entrena, aplica el modelo a testing, y devuelve el vector de probabilidades

OneTrainPredict <- function(param_completo) {
    
  modelo <- lgb.train(
    data= dtrain,
    param= param_completo
  )
  gmodelo <<- modelo

  # aplico el modelo a los datos nuevos
  pred <- predict(
    modelo,
    test_matrix
  )

  return( pred )
}

# En el argumento x llegan los parametros de la bayesiana
#  devuelve la ganancia en datos de testing

# aqui se ira guardando la mejor iteracion de la bayesiana
gmejor <- list()
gmejor$iter <- 0
gmejor$gan <- -Inf

giter <- 0
if( file.exists("BO_log.txt") ){
  tb_BO <- fread("BO_log.txt")
  giter <- nrow(tb_BO) -1 
}

EstimarGanancia_lightgbm <- function(x) {

  giter <<- giter + 1
  # x pisa (o agrega) a param_fijos
  param_completo <- modifyList(PARAM$lgbm$param_fijos, x)

  vgan_mesetas <- c()  # las ganancias, tengo repe de ellas

  # loop de las repeticionies
  for( repe in seq( PARAM$hipeparametertuning$repe ) )
  {
     desde <- (repe-1)*PARAM$hipeparametertuning$ksemillerio + 1
     hasta <- desde + PARAM$hipeparametertuning$ksemillerio -1
     rsemillas <- PARAM$BO$semillas[ desde:hasta ]

     # vector inicial de probabilidades
     vpred_acum <- rep( 0.0, nrow(dataset_test) )

     # loop del semillerio
     for( sem in rsemillas ) # itero semillerio
     {
        param_completo$seed <- sem  # asigno se semilla
        vpred_acum <- vpred_acum + OneTrainPredict( param_completo )
        
        gc(full= TRUE, verbose= FALSE)
     }

     # Calculo de ganancia suavizada de la meseta
     tb_prediccion <- dataset_test[, list(gan)]
     tb_prediccion[, prob := vpred_acum ]

     setorder(tb_prediccion, -prob)
     tb_prediccion[, gan_acum := cumsum(gan)]

     # la meseta es un punto, mil para la izquierda, otros mil para la derecha
     tb_prediccion[, gan_meseta :=
       frollmean(
         x= gan_acum, n= 2001, align= "center",
         na.rm= TRUE, hasNA= TRUE
      )
     ]

     vgan_mesetas <- c(vgan_mesetas, tb_prediccion[, max(gan_meseta, na.rm = TRUE)] )
  }

  gan_mesetas_prom <- mean( vgan_mesetas ) 

  if( gan_mesetas_prom > gmejor$gan ){
    gmejor$gan <<- gan_mesetas_prom
    gmejor$iter <<- giter

    # hrabo importancia de variables
    fwrite( lgb.importance(gmodelo),
      file= paste0("impo_", giter, ".txt"),
      sep= "\t"
    )
  }

  # datos qeu voy a loguear
  xx <- copy(param_completo)
  xx$iter <- giter
  xx$metrica_mejor <- gmejor$gan
  xx$metrica_sd <- sd(vgan_mesetas)
  xx$metrica <- gan_mesetas_prom

  loguear( xx, "BO_log.txt")
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")  # le reordeno a mlrMBO

  return( gan_mesetas_prom ) #tiempo_corrida) )
}

# Aqui comienza la configuracion de la Bayesian Optimization
#  es compleja la configuracion de una Bayesian Optimization

# en este archivo quedan la evolucion binaria de la BO
kbayesiana <- "bayesiana.RDATA"

funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output= FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo

obj.fun <- makeSingleObjectiveFunction(
  fn= funcion_optimizar, # la funcion que voy a maximizar
  minimize= FALSE, # estoy Maximizando la ganancia
  noisy= TRUE,
  par.set= PARAM$hipeparametertuning$hs, # definido al comienzo del programa
  has.simple.signature= FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time= 600, # se graba cada 600 segundos
  save.file.path= kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters= PARAM$hipeparametertuning$BO_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type= "se",
  covtype= "matern3_2",
  control= list(trace= TRUE)
)

Sys.time()

# inicio la optimizacion bayesiana, retomando si ya existe
# es la celda mas lenta de todo el notebook

if (!file.exists(kbayesiana)) {
  bayesiana_salida <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  bayesiana_salida <- mboContinue(kbayesiana) # retomo en caso que ya exista
}

Sys.time()

PARAM$train_final$future <- c(202107)

PARAM$train_final$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105
)

PARAM$train_final$undersampling <- 0.05

# se filtran los meses donde se entrena el modelo final
dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

# Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINIA"

set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset_train_final[, azar := runif(nrow(dataset_train_final))]
dataset_train_final[, training := 0L]

dataset_train_final[
  (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

dataset_train_final[, azar:= NULL] # elimino la columna azar

# paso la clase a binaria que tome valores {0,1}  enteros
#  BAJA+1 y BAJA+2  son  1,   CONTINUA es 0
#  a partir de ahora ya NO puedo cortar  por prob(BAJA+2) > 1/40

dataset_train_final[,
  clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
]

# leo el archivo donde quedaron los hiperparametros optimos
tb_BO <-  fread("BO_log.txt")
setorder( tb_BO, -metrica)  # ordeno por metrica descendente
tb_BO[1]

# en la tabla ademas de los parametros del LightGBM, hay campos de salida
param_lgbm <- union( names(PARAM$lgbm$param_fijos),  names(PARAM$hipeparametertuning$hs$pars) )

PARAM$train_final$param_mejores <- as.list( tb_BO[1, param_lgbm, with=FALSE])

PARAM$train_final$param_mejores$min_data_in_leaf <- as.integer( round(PARAM$train_final$param_mejores$min_data_in_leaf * nrow(dataset_train_final[training == 1L]) / nrow(dtrain)))

cat( tb_BO[1, min_data_in_leaf] , PARAM$train_final$param_mejores$min_data_in_leaf, "\n")
PARAM$train_final$param_mejores

# Semillerio Final
PARAM$train_final$APO <- 3
PARAM$train_final$ksemillerio  <- 3

PARAM$train_final$cortes <- c(10500, 11000, 11500, 12000)

# valvula de seguridad anti atajos
if( PARAM$train_final$APO * PARAM$train_final$ksemillerio  < 100
  | PARAM$train_final$APO < 5
)  {
  # stop("No quiera overfitear")
  cat("No quiera overfitear")
}

set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
PARAM$train_final$semillas <- sample(primos)[seq( PARAM$train_final$ksemillerio*PARAM$train_final$APO )]
PARAM$train_final$semillas

# dejo los datos en formato LightGBM
dtrain_final <- lgb.Dataset(
  data= data.matrix(dataset_train_final[training == 1L, campos_buenos, with= FALSE]),
  label= dataset_train_final[training == 1L, clase01],
  free_raw_data= FALSE
)

cat("filas", nrow(dtrain_final), "columnas", ncol(dtrain_final), "\n")
Sys.time()

# genero los modelitos
dir.create( "modelitos", showWarnings= FALSE)

param_completo <- copy( PARAM$train_final$param_mejores)

for( sem in PARAM$train_final$semillas ) {

  arch_modelo <- paste0("./modelitos/mod_", sem, ".txt")
  if( !file.exists( arch_modelo ) )
  {
    param_completo$seed <- sem

    modelito <- lgb.train(
      data= dtrain_final,
      param= param_completo
    )

    lgb.save( modelito, filename= arch_modelo)
    rm(modelito)
    gc()
  }
}

Sys.time()

# dataset de future, donde en este caso estoy haciendo testing
dfuture <- dataset[foto_mes %in% PARAM$train_final$future ]
mfuture <- data.matrix(dfuture[, campos_buenos, with= FALSE])

dfuture[, ganancia := ifelse(clase_ternaria=="BAJA+2", 780000, -20000)]

mganancias <- matrix( nrow=PARAM$train_final$APO, ncol= length(PARAM$train_final$cortes) )

if( file.exists("prediccion.txt") )
  file.remove("prediccion008.txt")

# aplico el modelo a los datos del future

for( vapo in seq(PARAM$train_final$APO) ) {
  # inicializacion en CERO
  vpred_acum <- rep(0.0, nrow(dfuture))
  qacumulados <- 0

  desde <- 1 + (vapo-1)*PARAM$train_final$ksemillerio
  hasta <- desde + PARAM$train_final$ksemillerio - 1
  semillas <- PARAM$train_final$semillas[desde:hasta]

  for( sem in semillas ) {

    arch_modelo <- paste0("./modelitos/mod_", sem, ".txt")
    if( file.exists( arch_modelo ) )
    {
      modelo_final <- lgb.load(arch_modelo) # leo del disco
      # hago el predict() y acumulo
      vpred_acum <- vpred_acum + predict(modelo_final, mfuture)
      qacumulados <- qacumulados + 1
      rm(modelo_final)
      gc()
    }
  }

  if( qacumulados > 0 ) {
    vpred_acum <- vpred_acum / qacumulados  # paso a probabildiad
    # tabla de prediccion, puede ser util para futuros ensembles
    #  ya que le modelo ganador va a ser un ensemble de LightGBMs

    tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, ganancia)]
    tb_prediccion[, meta_modelo := vapo]
    tb_prediccion[, prob := vpred_acum ]
    setorder( tb_prediccion, -prob )
    tb_prediccion[, gan_acum := cumsum(ganancia)]
    tb_prediccion[, ganancia := NULL ]

    # acumulo las ganancias
    for( icor in seq(length(PARAM$train_final$cortes)) ){
      mganancias[ vapo, icor ] <- tb_prediccion[ PARAM$train_final$cortes[icor], gan_acum ]
    }

    # grabo las probabilidades del modelo
    fwrite(tb_prediccion,
      file= "prediccion010.txt",
      sep= "\t",
      append= TRUE
    )

    rm(tb_prediccion)
    gc()
  }
}

Sys.time()

mganancias

# genero archivos con los  "envios" mejores
dir.create("kaggle", showWarnings=FALSE)

tb_prediccion <- fread("prediccion.txt")

# genero archivos de fantasia, que NO son el que voy a subir a la Pseudo Competencia Kaggle
envios <- 11000

for( vapo in seq(PARAM$train_final$APO) ) {
  if( tb_prediccion[meta_modelo==vapo, .N] > 0 ) {
    tb_pred <- tb_prediccion[meta_modelo==vapo]
    setorder( tb_pred, -prob )
    tb_pred[, Predicted := 0L] # seteo inicial a 0
    tb_pred[1:envios, Predicted := 1L] # marco los primeros

    archivo_kaggle <- paste0("./kaggle/KA", PARAM$experimento, "_", vapo, "_", envios, ".csv")

    # grabo el archivo
    fwrite(tb_pred[, list(numero_de_cliente, Predicted)],
      file= archivo_kaggle,
      sep= ","
    )

    rm( tb_pred )
    gc()
  }
}

Sys.time()

