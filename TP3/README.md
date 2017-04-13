# TP3 Detección de fallas en Erlang


El TP es un pasaje de mensajes, donde se muestra el incremento de un contador respecto a un delay de tiempo. Además de observar que el envió de mensajes no garantiza que se reciba correctamente.

Se utilizan 2 módulos:

- Producer: Este módulo creara un nuevo proceso producer registrándolo con el nombre de "producer", el cual esperara un mensaje del "consumer". Cuando esto suceda el producer interactuara con el consumer, enviándole números sucesivos por cada uno de los mensajes. En caso de que haya un mensaje STOP desde el consumer, el producer le avisara con un "bye" que la conversación finalizó. Por otro lado, si ocurre un crash no tendrá tiempo para avisarle al consumer.

- Consumer: Este módulo, a diferencia del Producer, creara un proceso consumer pasándole como parametro al producer con el que entablara la conversación. Una vez instanciado le enviara al producer un mensaje "hello" para comenzar el envío de mensajes simultaneo. El consumer revisara cada uno de los mensajes enviados por el producer e imprimirá un string distinto dependiendo el resultado. Además, sabra como finalizar su envío de mensajes cuando el producer lo notifique. En el caso de que el producer haga crash, el consumer tendrá un cierto tiempo de tolerancia antes de finalizar.

### 3.1. En el mismo host
	1. ¿Qué mensaje se da como razón cuando el nodo es terminado? ¿Por qué?
		Se termina correctamente, porque se hace referencia correcta al proceso Producer, por lo tanto están bien sincronizados los procesos.

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
