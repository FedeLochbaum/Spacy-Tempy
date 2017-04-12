# TP3 Detecci�n de fallas en Erlang*
---

El TP es un paso de mensaje, donde se muestra el incremento de un contador respecto
a un delay de tiempo. Adem�s de observar que el envi� de mensajes no garantiza que se reciba
correctamente.

Se utilizan 2 m�dulos:

- Producer: Ser�a como el servidor en esta pr�ctica porque es el que le manda el
	mensaje con el nuevo valor al contador para el Consumer. En este m�dulo para inicializarlo
	le tenemos que asignar un valor de delay; adem�s de que tiene funciones
	donde hace que el proceso de Producer se detenga (stop) o crashie (crash).

- Consumer: Este m�dulo para inicializarlo le tenemos que asignar un Producer, al
	hacer eso se crea un proceso Consumer, el cual se registra y se comunica
	con el Producer, donde el Producer lo recibe, espera el tiempo de Delay
	que se le asigno al registrar el Producer y al pasar ese tiempo le pasa
	al Consumer el nuevo n�mero.


### 3.1. En el mismo host
	1. �Qu� mensaje se da como raz�n cuando el nodo es terminado? �Por qu�?
		Se termina correctamente, porque se hace referencia correcta al 
		proceso Producer, por lo tanto est�n bien sincronizados los procesos.
	
### 3.2. Un experimento distribuido
	1. �Qu� sucede si matamos el nodo Erlang en el producer?
		El proceso Consumer muere.

	2. Ahora probemos desconectar el cable de red de la m�quina corriendo el producer y volvamos a enchufarlo despues de unos segundos. �Qu� pasa? 
		El proceso Consumer muere porque no hay conexi�n con el Producer.

	3. Desconectemos el cable por per�odos mas largos. �Qu� pasa ahora?
		Igual que el punto anterior, el proceso Consumer muere por falta de conexi�n con el proceso Producer.

	4. �Qu� significa haber recibido un mensaje �DOWN�? �Cu�ndo debemos confiar en el?
		No se recibi�.

	5. �Se recibieron mensajes fuera de orden, aun sin haber recibido un mensaje �DOWN�? 
		No.

	6. �Qu� dice el manual acerca de las garant�as de env�os de mensajes?
		Se env�a el mensaje pero no hay garant�a en que llegue.