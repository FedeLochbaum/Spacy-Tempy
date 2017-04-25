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


#### 6. ¿Qué rápido podemos operar sobre el store?


#### 7. ¿Qué sucede si hacemos esto en una red de Erlang distribuida, qué es lo
que se copia cuando el handler de transacciones arranca?


#### 8. ¿Dónde corre el handler?


#### 9. ¿Cuales son los pros y contras de la estrategia de implementación?