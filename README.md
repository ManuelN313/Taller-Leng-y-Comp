# Documentación del Intérprete en Haskell

Este trabajo de la materia Lenguajes y Compiladores presenta un intérprete para un lenguaje imperativo simple, implementado en Haskell. Este lenguaje incluye variables, expresiones aritmético-lógicas, control de flujo (condicionales, bucles), manejo de errores y operaciones de entrada/salida.


## 1. Estructura general del intérprete

El intérprete está estructurado en tres capas principales:

### 1.1. Representación del lenguaje (AST)

Se usa un GADT `Expr a` para representar:

* **Expresiones aritméticas y booleanas** (`Expr MaybeInt`, `Expr MaybeBool`)
* **Comandos o instrucciones** (`Expr Ω`)

Esta estructura permite que cada tipo de expresión esté fuertemente tipada, facilitando la implementación segura de la semántica.

### 1.2. Semántica denotacional

Se define una clase de tipo `DomSem dom` con la función:

```haskell
sem :: Expr dom -> Σ -> dom
```

Cada tipo (`MaybeInt`, `MaybeBool`, `Ω`) tiene una instancia que implementa la semántica correspondiente.

Particularmente, la instancia `DomSem Ω` implementa la semántica de los comandos imperativos:

* Asignaciones
* Secuencias
* Condicionales
* Bucle while
* Manejo de errores (`fail`, `catch`)
* Entrada/Salida (`SysOut`, `SysIn`)

### 1.3. Modelo de ejecución (Ω)

El tipo `Ω` representa los posibles resultados de ejecutar un programa:

* `Normal σ`: terminación normal
* `Abort σ`: error en tiempo de ejecución
* `Out (Int, Ω)`: salida de un valor
* `In (Int -> Ω)`: espera entrada


## 2. Funciones auxiliares clave

El intérprete define combinadores semánticos como:

* `>>==`: bind extendido para propagación de errores con estado
* `(*.)`: composición secuencial de comandos
* `(+.)`: manejo de errores con `catch`
* `(†.)`: restauración de estado (para variables locales)
* `fix`: definición de bucles (`while`) como punto fijo

Estas funciones encapsulan patrones comunes de evaluación y son esenciales para expresar la semántica de forma modular y clara.

---

## 3. Entrada/Salida y evaluación

La clase `Eval` permite ejecutar expresiones interactivamente en la consola. Para `Ω`, se define la función `unroll`, que procesa estructuras anidadas de salida/entrada hasta llegar a una terminación.

---

## 4. Conclusión

Este intérprete es un ejemplo clásico de cómo implementar la semántica denotacional de un lenguaje imperativo en Haskell, usando GADTs, funciones de orden superior y manejo explícito de errores y efectos.

La estructura del programa separa claramente la representación del lenguaje, su semántica y su ejecución, haciendo el diseño limpio y extensible.
