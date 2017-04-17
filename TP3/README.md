# TP3 Deteccion de fallas en Erlang


El TP es un pasaje de mensajes, donde se muestra el incremento de un contador respecto a un delay de tiempo. Ademas de observar que el envia de mensajes no garantiza que se reciba correctamente.

Se utilizan 2 modulos:

- Producer: Este modulo creara un nuevo proceso producer registrandolo con el nombre de "producer", el cual esperara un mensaje del "consumer". Cuando esto suceda el producer interactuara con el consumer, enviandole numeros sucesivos por cada uno de los mensajes. En caso de que haya un mensaje STOP desde el consumer, el producer le avisara con un "bye" que la conversacion finalizo. Por otro lado, si ocurre un crash no tendra tiempo para avisarle al consumer.

- Consumer: Este modulo, a diferencia del Producer, creara un proceso consumer pasandole como parametro al producer con el que entablara la conversacion. Una vez instanciado le enviara al producer un mensaje "hello" para comenzar el envio de mensajes simultaneo. El consumer revisara cada uno de los mensajes enviados por el producer e imprimira un string distinto dependiendo el resultado. Ademas, sabra como finalizar su envio de mensajes cuando el producer lo notifique. En el caso de que el producer haga crash, el consumer tendra un cierto tiempo de tolerancia antes de finalizar.

### 3.1. En el mismo host
#### ¿Que mensaje se da como razon cuando el nodo es terminado? ¿Por que?
Aunque esten en diferentes nodos, si el producer es finalizado por medio de un stop(), el consumer recibe el mensaje y finaliza correctamente.
De otra manera, si el producer es finalizado mediante un crash, el mensaje obtenido por el consumer es:

``` "died; {badarith,[{producer,producer,3,[{file,[112,114,111,100,117,99,101,114,46,101,114,108]},{line,27}]}]}". ```

Pero, si el nodo donde corre el consumer es finalizado con una excepcion de sistema, el mensaje obtenido por el consumer es:

```"died; noconnection". ```

Es decir que se perdio la conexion entre nodos.

### 3.2. Un experimento distribuido
#### 1. ¿Que sucede si matamos el nodo Erlang en el producer?
Utilizando los nodos desde diferentes computadoras.Si el producer es finalizado matando su nodo, el consumer devuelve la siguiente respuesta:

```"{producer, silver@192.168.183.129} died; noconnection". ```

Claramente puede verse que a diferencia del punto anterior, este especifica el nodo finalizado.

#### 2. Ahora probemos desconectar el cable de red de la maquina corriendo el producer y volvamos a enchufarlo despues de unos segundos. ¿Que pasa?
En este caso el proceso consumer se queda esperando la respuesta del producer, cuando este vuelve a conectarse se reestablece el envio de mensajes y continuan interactuando.

#### 3. Desconectemos el cable por periodos mas largos. ¿Que pasa ahora?
En un principio la conexion se reestablece, pero al desconectarlo por mas de 20 segundos, es consumer deja de esperar al producer e imprime el mismo mensaje de error que cuando se mataba el nodo:

```"{producer, silver@192.168.183.129} died; noconnection". ```

Y el producer:

```"Node 'gold@192.168.43.236' not responding".```

```"Removing (timedout) connection"```

#### 4. ¿Que significa haber recibido un mensaje 'DOWN'? ¿Cuando debemos confiar en el?
Haber recibido el mensaje 'DOWN' significa que la conexion con otro nodo se perdio y no puede ser recuperada. Cuando desconectabamos el producer por un periodo de tiempo corto, la conexion se retomaba sin haber imprimido mensaje alguno desde el consumer.

#### 5. ¿Se recibieron mensajes fuera de orden, aun sin haber recibido un mensaje 'DOWN'?
En ningun caso se recibieron mensajes fuera de orden desde el consumer. Todo el tiempo estuvo a "timing" con el producer.

#### 6. ¿Que dice el manual acerca de las garantias de envios de mensajes?
La documentacion de erlang dice que los mensajes se envian correctamente pero no existe garantia de que se reciban de la misma manera.
