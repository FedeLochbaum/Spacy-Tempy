# TP3 Deteccion de fallas en Erlang


El TP es un pasaje de mensajes, donde se muestra el incremento de un contador respecto a un delay de tiempo. Ademas de observar que el envia de mensajes no garantiza que se reciba correctamente.

Se utilizan 2 modulos:

- Producer: Este modulo creara un nuevo proceso producer registrandolo con el nombre de "producer", el cual esperara un mensaje del "consumer". Cuando esto suceda el producer interactuara con el consumer, enviandole numeros sucesivos por cada uno de los mensajes. En caso de que haya un mensaje STOP desde el consumer, el producer le avisara con un "bye" que la conversacion finalizo. Por otro lado, si ocurre un crash no tendra tiempo para avisarle al consumer.

- Consumer: Este modulo, a diferencia del Producer, creara un proceso consumer pasandole como parametro al producer con el que entablara la conversacion. Una vez instanciado le enviara al producer un mensaje "hello" para comenzar el envio de mensajes simultaneo. El consumer revisara cada uno de los mensajes enviados por el producer e imprimira un string distinto dependiendo el resultado. Ademas, sabra como finalizar su envio de mensajes cuando el producer lo notifique. En el caso de que el producer haga crash, el consumer tendra un cierto tiempo de tolerancia antes de finalizar.

### 3.1. En el mismo host
#### ¿Que mensaje se da como raz�n cuando el nodo es terminado? ¿Por que?
Aunque esten en diferentes nodos, si el producer es finalizado por medio de un stop(), el consumer recibe el mensaje y finaliza correctamente.
De otra manera, si el producer es finalizado mediante un crash, el mensaje obtenido por el consumer es:
	1. "died; {badarith,[{producer,producer,3,[{file,[112,114,111,100,117,99,101,114,46,101,114,108]},{line,27}]}]}".
Pero, si el nodo donde corre el consumer es finalizado con una excepcion de sistema, el mensaje obtenido por el consumer es:
	2. "died; noconnection".
Es decir que se perdio la conexion entre nodos.

### 3.2. Un experimento distribuido
	1. ¿Que sucede si matamos el nodo Erlang en el producer?
		El proceso Consumer muere.

	2. Ahora probemos desconectar el cable de red de la maquina corriendo el producer y volvamos a enchufarlo despues de unos segundos. ¿Que pasa?
		El proceso Consumer muere porque no hay conexion con el Producer.

	3. Desconectemos el cable por per�odos mas largos. ¿Que pasa ahora?
		Igual que el punto anterior, el proceso Consumer muere por falta de conexi�n con el proceso Producer.

	4. ¿Que significa haber recibido un mensaje 'DOWN'? ¿Cuando debemos confiar en el?
		No se recibio.

	5. ¿Se recibieron mensajes fuera de orden, aun sin haber recibido un mensaje 'DOWN'?
		No.

	6. ¿Que dice el manual acerca de las garantias de envios de mensajes?
		Se envia el mensaje pero no hay garantia en que llegue.
