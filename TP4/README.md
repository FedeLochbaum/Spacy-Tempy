# TP4 Opty: control concurrencia optimista

El control de bloqueo optimista o control de concurrencia optmista nos permite reducir el nivel de aislamiento que se utiliza en una aplicación. 
Este tipo de control permite que varios usuarios intenten actualizar el mismo registro sin informar a los usuarios de que otros también están intentando actualizar el registro.

Un bloqueo optimista es que evista la sobrecarga del bloque de un registro durante la duración de la acción.

### Performance

#### 1. ¿Performa?


#### 2. ¿Cuántas transacciones podemos hacer por segundo?


#### 3. ¿Cuales son las limitaciones en el número de transacciones concurrentes y la tasa de éxito?


#### 4. ¿Algo más?


#### 5. ¿Es realista la implementación del store que tenemos?
Si, porque ayuda a mantener el código más limpio, separado por módulos y con esto es mejor hacer modificaciones facilmente en caso de que se requiera.

#### 6. ¿Qué rápido podemos operar sobre el store?


#### 7. ¿Qué sucede si hacemos esto en una red de Erlang distribuida, qué es lo que se copia cuando el handler de transacciones arranca?
Copia el Client, Validator y el Store.

#### 8. ¿Dónde corre el handler?
Corre del lado del Cliente

#### 9. ¿Cuales son los pros y contras de la estrategia de implementación?

##### Pros
- El uso del Handler ayuda a realizar todas las operaciones: read, write
- El Validator como su nombre lo indica es el que valida si el archivo ha sido moficado o no, si sí, entonces hace la escritura. 
- El store que es donde almacena los datos, hace los escrituras directamente al archivo y también toma un dato en caso de necesitarlo.

##### Contra
- 