# DSL para Base de Datos NoSQL

## Descripción General

Este proyecto consiste en el desarrollo de un DSL (Domain Specific Language) inspirado en sistemas de bases de datos documentales tipo NoSQL. El lenguaje permite crear, modificar, consultar y administrar colecciones de documentos JSON mediante una sintaxis propia.

El objetivo principal del proyecto es modelar un pequeño motor de consultas y manipulación de datos utilizando herramientas clásicas de construcción de compiladores e intérpretes en Haskell.

El DSL fue desarrollado utilizando un Árbol de Sintaxis Abstracta (AST), un parser construido con Parsec y un evaluador encargado de ejecutar los comandos definidos en el lenguaje.

---

# Objetivos

- Diseñar un lenguaje especializado para manipular bases de datos documentales.
- Implementar un parser capaz de reconocer programas escritos en el DSL.
- Ejecutar operaciones sobre colecciones y documentos JSON.
- Incorporar manejo de errores semánticos y de ejecución.
- Permitir persistencia de vistas y snapshots de la base de datos.
- Aplicar conceptos de programación funcional y diseño de intérpretes en Haskell.

---

# Estructura del Proyecto

El proyecto se encuentra dividido en múltiples módulos:

- **AST.hs**  
  Define la estructura del Árbol de Sintaxis Abstracta utilizada por el lenguaje.

- **Parser.hs**  
  Implementa el parser utilizando Parsec. Convierte programas escritos en el DSL a su representación AST.

- **Evaluator.hs**  
  Implementa el evaluador/interprete del lenguaje. Ejecuta los comandos sobre el estado interno de la base de datos.

- **Value.hs**  
  Define los tipos de valores soportados por el lenguaje, incluyendo números, strings, booleanos, arrays, objetos y null.

- **JSONAdapter.hs**  
  Se encarga de convertir estructuras internas del DSL a JSON y viceversa.

- **Main.hs**  
  Punto de entrada principal del proyecto. Coordina parsing, evaluación y ejecución de programas.

---

# Funcionalidades Soportadas

## Manejo de Colecciones

- Creación de colecciones.
- Eliminación de colecciones.
- Inserción individual de documentos.
- Inserción múltiple de documentos.
- Actualización de documentos.
- Eliminación de documentos mediante filtros.

### Ejemplos

```txt
createCollection.logs();
insert.logs(
  { 
    msg :s "inicio", 
    date :s "7/5/2026" 
  }
);
```

---

# Consultas

El DSL permite realizar consultas sobre colecciones mediante pipelines de operaciones.

## Operaciones Soportadas

- FILTER
- SELECT
- SORT
- LIMIT
- GROUP
- HAVING

## Ejemplo

```txt
find.users()
.filter(
  exists(direccion.calle) && country eqS "AR"
)
.select(name, age, country)
.sort({age: desc})
.limit(5)
.preview();
```

---

# Tipos de Datos Soportados

El lenguaje soporta múltiples tipos de datos:

- Int
- Float
- String
- Bool
- Null
- Arrays
- Objetos JSON anidados

## Ejemplo

```txt
{
  nombre:s: "Roman",
  edad:n: 22,
  activo:b: true,
  direccion:p: {
    "ciudad":s: "Rosario"
  }
}
```

---

# Expresiones Soportadas

## Expresiones Numéricas

- Suma
- Resta
- Multiplicación
- División

## Expresiones Booleanas

- AND
- OR
- NOT
- Comparaciones numéricas
- Comparaciones de strings
- Comparaciones booleanas
- EXISTS
- IS NULL

## Acceso a Campos

El lenguaje soporta acceso a campos anidados mediante path expressions.

### Ejemplo

```txt
usuario.direccion.ciudad
```

---

# Agregaciones

Las operaciones GROUP soportan agregaciones similares a las de sistemas NoSQL reales.

## Agregaciones Disponibles

- COUNT
- SUM
- AVG
- MIN
- MAX

## Ejemplo

```txt
find.users()
.filter( exists(country) && direccion.tipo eqS "departamento" )
.groupby(country)
.count("totalUsuarios", name)
.avg("promedioEdad", age)
.having(totalUsuarios > 0)
.preview()
```

---

# Vistas

El DSL permite guardar consultas persistentes llamadas vistas.

## Funcionalidades

- Crear vistas.
- Ejecutar vistas.
- Extender vistas agregando nuevas etapas al pipeline.

## Ejemplo

```txt
createView("mayores",
  find.users()
    .filter(!(age < 23 + 2))
    .preview()
);

useView("mayores")
```

---

# Snapshots y Rollback

El sistema soporta snapshots de colecciones o de toda la base de datos.

## Funcionalidades

- Crear timestamps.
- Restaurar snapshots.
- Rollback de colecciones.
- Rollback completo de la base de datos.

---

# Transacciones

El lenguaje incorpora soporte básico de transacciones.

Si ocurre un error durante la ejecución de una transacción, el estado previo se restaura automáticamente.

## Ejemplo

```txt
transaction{
  insertMany.users([
    {name:s "Carlos", age:n 35, active:b true, country:s "AR", score:n 13},
    {name:s "Paula", age:n 28, active:b true, country:s "AR", score:n 17},
    {name:s "Rocio", age:n 26, active:b false, country:s "AR", score:n 24}
  ]);

  updateOne.users(
    name eqS "Roman",
    {category:s "gold"}
  );

  delete.users(age < 19)

}

```

En este caso la transacción completa se revierte debido al error.

---

# Manejo de Errores

El evaluador implementa manejo explícito de errores semánticos y de ejecución.

## Algunos errores contemplados

- Colección inexistente.
- Vista inexistente.
- Timestamp inexistente.
- División por cero.
- Error de tipos.
- Uso inválido del campo reservado `_id`.
- Acceso a campos inexistentes.
- Creación duplicada de colecciones o vistas.

---

# Persistencia

El proyecto utiliza archivos JSON para almacenar:

- Vistas.
- Snapshots/Timestamps.

Esto permite mantener información persistente entre ejecuciones.

---

# Instrucciones de Uso

## Requisitos

- GHC 9.x o superior.
- Cabal o Stack.
- Librería `parsec`.
- Librería `aeson`.
- Librería `aeson-pretty`.

---

## Ejecución

Compilar el proyecto:

```bash
runhaskell Main.hs ejemplos/delete.lis database.json 
```
