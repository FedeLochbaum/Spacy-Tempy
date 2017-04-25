# TP4 Opty: control concurrencia optimista

El control de bloqueo optimista o control de concurrencia optmista nos permite reducir el nivel de aislamiento que se utiliza en una aplicaci�n. 
Este tipo de control permite que varios usuarios intenten actualizar el mismo registro sin informar a los usuarios de que otros tambi�n est�n intentando actualizar el registro.

Un bloqueo optimista es que evista la sobrecarga del bloque de un registro durante la duraci�n de la acci�n.

### Performance

#### 1. �Performa?


#### 2. �Cu�ntas transacciones podemos hacer por segundo?


#### 3. �Cuales son las limitaciones en el n�mero de transacciones concurrentes y la tasa de �xito?


#### 4. �Algo m�s?


#### 5. �Es realista la implementaci�n del store que tenemos?


#### 6. �Qu� r�pido podemos operar sobre el store?


#### 7. �Qu� sucede si hacemos esto en una red de Erlang distribuida, qu� es lo
que se copia cuando el handler de transacciones arranca?


#### 8. �D�nde corre el handler?


#### 9. �Cuales son los pros y contras de la estrategia de implementaci�n?