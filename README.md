# MiApp

[![Build binary](https://github.com/user/repo/actions/workflows/build-binary.yaml/badge.svg)](https://github.com/user/repo/actions/workflows/build-binary.yaml)

MiApp es un paquete R que encapsula una aplicación Shiny interactiva para el análisis de datos bioquímicos y curvas de crecimiento. Esta refactorización organiza el código de la antigua `app.R` en módulos más mantenibles dentro de `R/` y aloja la app completa en `inst/app/`.

## Instalación

Puedes instalar la versión binaria generada por las GitHub Actions descargando el artefacto más reciente y ejecutando:

```r
install.packages("MiApp_x.y.z.zip", repos = NULL)
```

Para instalar desde el código fuente:

```r
install.packages("MiApp_x.y.z.tar.gz", repos = NULL, type = "source")
```

## Uso

Tras la instalación, inicia la aplicación con:

```r
MiApp::run_app()
```

El paquete incluye todos los recursos estáticos en `inst/app/www/` así como las funciones auxiliares separadas en varios scripts de R.

## Desarrollo

Este repositorio utiliza `devtools`, `testthat` y `usethis` para el flujo de trabajo de paquete. Los binarios para Windows y las tarballs de código fuente se construyen de manera automática mediante GitHub Actions.

