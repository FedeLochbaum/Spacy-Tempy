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
Si, porque ayuda a mantener el c�digo m�s limpio, separado por m�dulos y con esto es mejor hacer modificaciones facilmente en caso de que se requiera.

#### 6. �Qu� r�pido podemos operar sobre el store?


#### 7. �Qu� sucede si hacemos esto en una red de Erlang distribuida, qu� es lo que se copia cuando el handler de transacciones arranca?
Copia el Client, Validator y el Store.

#### 8. �D�nde corre el handler?
Corre del lado del Cliente

#### 9. �Cuales son los pros y contras de la estrategia de implementaci�n?

##### Pros
- El uso del Handler ayuda a realizar todas las operaciones: read, write
- El Validator como su nombre lo indica es el que valida si el archivo ha sido moficado o no, si s�, entonces hace la escritura. 
- El store que es donde almacena los datos, hace los escrituras directamente al archivo y tambi�n toma un dato en caso de necesitarlo.

##### Contra
- 