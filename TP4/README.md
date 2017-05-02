# TP4 Opty: control concurrencia optimista

El control de bloqueo optimista o control de concurrencia optmista nos permite reducir el nivel de aislamiento que se utiliza en una aplicación.
Este tipo de control permite que varios usuarios intenten actualizar el mismo registro sin informar a los usuarios de que otros también están intentando actualizar el registro.

Un bloqueo optimista es que evista la sobrecarga del bloque de un registro durante la duración de la acción.

### Performance

#### 1. ¿Performa?
La ejecución de cada instrucción se realiza de forma rápida, solo en el caso del Commit tarda un poco más, y esto es debido a que se debe validar el Commit.

Para corroborar esto, optamos por crear un test, similar al brindado por los docentes en un tp anterior. En principio decimos verificar cual era el tiempo de N lecturas que se hacian de manera secuencial y luego de manera concurrente. Vimos que de manera concurrente el tiempo de respuesta era mucho mayor.
Para complejizar la prueba del sistema, decidimos crear automaticamente un numero aleatorio de clientes que se encarguen de leer, escribir y commitear cocurrentemente. Distribuimos la responsabilidad de generar y accionar cada tipo de cliente en un proceso aparte. Asi luego el proceso principal recolectaria el tiempo total que tardaron los clientes en cada caso.
A pesar de utilizar un numero muy grande de clientes el tiempo de respuesta era realmente rapido.



#### 2. ¿Cuántas transacciones podemos hacer por segundo?
Como dijimos anteriormente, gracias a nuestro test, podemos verificar una cantidad enorme de procesos tratando de leer, escribir y commitear al mismo tiempo. Por ejemplo para un test donde se crean de manera aleatoria entre 1 y N = 100000 de clientes de  cada tipo con 1000 entradas el resultado es el siguiente:
> - Cantidad de clientes lectores: 77530
- Cantidad de clientes escritores: 27512
- Cantidad de clientes commiteadores: 44980
- Cantidad total de clientes: 150022
- Cantidad de entradas: 1000
- Promedio de lecturas por segundo: 2.141184057784084e-5
- Promedio de escrituras por segundo: 2.7760977028205873e-5
- Promedio de commits por segundo: 1.7933459315251224e-5
- Tardo en segundos: 1.747272


Recordar que como habiamos comentado, no se crea un total de N clientes sino, un numero aleatorio entre 1 y N por cada operacion, es decir read, write y commit que intentan hacer sus transacciones al mismo tiempo.

#### 3. ¿Cuales son las limitaciones en el número de transacciones concurrentes y la tasa de éxito?
Pese a que nuestro en nuestro test no recolecta los fallos, no podemos sacar un % de tasa de exito certera. Pero podriamos decir, en base a test manuales que hemos hecho que con 1 millón o más de usuarios (proceos) haciendo read, write y commit comienza a reportar error para poca cantidad de entradas.

#### 4. ¿Algo más?
En el store no se soportan muchos registros, el máximo que puede soportar es dependiendete de la memoria del equipo no llega a 100,000.
Aun asi, con la ayuda distribuida de nuestro test, pudimos corroborar casos donde el numero entradas supera el dicho anteriormente.

#### 5. ¿Es realista la implementación del store que tenemos?
Si, porque ayuda a mantener el código más limpio, separado por módulos y con esto es mejor hacer modificaciones facilmente en caso de que se requiera.

#### 6. ¿Qué rápido podemos operar sobre el store?
Es rápido porque no depende de otro módulo, solo cuando se le solicita añade o toma de la lista.

#### 7. ¿Qué sucede si hacemos esto en una red de Erlang distribuida, qué es lo que se copia cuando el handler de transacciones arranca?
Cuando el handler de transacciones arranca, se copia el Client, Validator y el Store.

#### 8. ¿Dónde corre el handler?
Corre del lado del Cliente

#### 9. ¿Cuales son los pros y contras de la estrategia de implementación?

##### Pros
- El uso del Handler ayuda a realizar todas las operaciones: read, write
- El Validator como su nombre lo indica es el que valida si el archivo ha sido moficado o no, si sí, entonces hace la escritura.
- El store que es donde almacena los datos, hace los escrituras directamente al archivo y también toma un dato en caso de necesitarlo.

##### Contra
- Hubo varias modificaciones para hacer funcionar correctamente el Test.
