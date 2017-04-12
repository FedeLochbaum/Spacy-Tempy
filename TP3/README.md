# TP3 Detección de fallas en Erlang*
---

El TP es un paso de mensaje, donde se muestra el incremento de un contador respecto
a un delay de tiempo. Además de observar que el envió de mensajes no garantiza que se reciba
correctamente.

Se utilizan 2 módulos:

- Producer: Sería como el servidor en esta práctica porque es el que le manda el
	mensaje con el nuevo valor al contador para el Consumer. En este módulo para inicializarlo
	le tenemos que asignar un valor de delay; además de que tiene funciones
	donde hace que el proceso de Producer se detenga (stop) o crashie (crash).

- Consumer: Este módulo para inicializarlo le tenemos que asignar un Producer, al
	hacer eso se crea un proceso Consumer, el cual se registra y se comunica
	con el Producer, donde el Producer lo recibe, espera el tiempo de Delay
	que se le asigno al registrar el Producer y al pasar ese tiempo le pasa
	al Consumer el nuevo número.


### 3.1. En el mismo host
	1. ¿Qué mensaje se da como razón cuando el nodo es terminado? ¿Por qué?
		Se termina correctamente, porque se hace referencia correcta al 
		proceso Producer, por lo tanto están bien sincronizados los procesos.
	
### 3.2. Un experimento distribuido
	1. ¿Qué sucede si matamos el nodo Erlang en el producer?
		El proceso Consumer muere.

	2. Ahora probemos desconectar el cable de red de la máquina corriendo el producer y volvamos a enchufarlo despues de unos segundos. ¿Qué pasa? 
		El proceso Consumer muere porque no hay conexión con el Producer.

	3. Desconectemos el cable por períodos mas largos. ¿Qué pasa ahora?
		Igual que el punto anterior, el proceso Consumer muere por falta de conexión con el proceso Producer.

	4. ¿Qué significa haber recibido un mensaje ’DOWN’? ¿Cuándo debemos confiar en el?
		No se recibió.

	5. ¿Se recibieron mensajes fuera de orden, aun sin haber recibido un mensaje ’DOWN’? 
		No.

	6. ¿Qué dice el manual acerca de las garantías de envíos de mensajes?
		Se envía el mensaje pero no hay garantía en que llegue.