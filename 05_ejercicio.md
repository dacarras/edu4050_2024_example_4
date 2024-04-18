Code 05: ejercicio
================
dacarras
Abril 18, 2024

<style>
  .main-container {
    max-width: 1600px !important;
  }
  .list-group-item.active, 
  .list-group-item.active:focus, 
  .list-group-item.active:hover {
    background-color: #373334;
  }
</style>

# Ejercicio

- Prepare los datos
  - filtre los casos de Mexico
  - genere una muestra de calibracion (exploratoria) de 500 casos
  - genere una muestra de confirmatoria de 500 casos
  - genere una muestra total de casos
  - seleccione los mismos items empleados en los ejemplos anterioes

<!-- -->


    #------------------------------------------------
    # item selection
    #------------------------------------------------

    items_selected <- read.table(
    text="
    var_name  variable item_text
    IS3G22D   td4      'All adult citizens have the right to elect their political leaders.'
    IS3G22I   td9      'All <ethnic/racial> groups in the country have the same rights.'
    IS3G22E   td5      'People are able to protest if they think a law is unfair.'
    IS3G22H   td8      '[R] The government influences decisions by courts of justice.'
    IS3G22B   td2      '[R] One company or the government owns all newspapers in a country.'
    ",
    header=TRUE, stringsAsFactors = FALSE)

    # display item table
    knitr::kable(items_selected)

- Numero de clases
  - ajuste 2-4 clases sobre la muestra exploratoria
  - ajuste 2-4 clases sobre la muestra confirmatoria
  - decida el numero de clases
- Resultados
  - Ajuste un modelo de 3 clases sobre la muestra total
  - Genere un gráfico de perfil de respuesta
  - Genere un gráfico de error de clasificación
- Bonus
  - Compare como se ven los errores de clasificación según la cantidad
    de observaciones con respuestas válidas.
    - ¿Que le pasa al error de clasificación según la cantidad de
      respuestas observadas?
  - Compare un modelo de 3 y 4 clases.
    - Ajuste ambos modelos
    - Agregue las realizaciones sobre la base de datos de respuestas
    - Genere una tabla cruzada de las realizaciones producidas
    - ¿Tiene sentido agregar una clase adicional?

# Preparación de datos

## Caja para código

``` r
#--------------------------------------------------------------------
# titulo
#--------------------------------------------------------------------

#------------------------------------------------
# subtitulo
#------------------------------------------------

# [ESCRIBIR CODIGO AQUI]
```

# Número de clases

## Caja para código

``` r
#--------------------------------------------------------------------
# titulo
#--------------------------------------------------------------------

#------------------------------------------------
# subtitulo
#------------------------------------------------

# [ESCRIBIR CODIGO AQUI]
```

# Resultados

## Caja para código

``` r
#--------------------------------------------------------------------
# titulo
#--------------------------------------------------------------------

#------------------------------------------------
# subtitulo
#------------------------------------------------

# [ESCRIBIR CODIGO AQUI]
```

# Perfil de respuesta

## Caja para código

``` r
#--------------------------------------------------------------------
# titulo
#--------------------------------------------------------------------

#------------------------------------------------
# subtitulo
#------------------------------------------------

# [ESCRIBIR CODIGO AQUI]
```

# Error de clasificación

## Caja para código

``` r
#--------------------------------------------------------------------
# titulo
#--------------------------------------------------------------------

#------------------------------------------------
# subtitulo
#------------------------------------------------

# [ESCRIBIR CODIGO AQUI]
```
