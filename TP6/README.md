# TP6 Loggy: Un logger de tiempo lógico

### 3 El Test

#### 1. Corramos algunos tests y tratemos de encontrar mensajes de log que sean mostrados fuera de orden. ¿Cómo sabemos que fueron impresos fuera de orden?

Como podemos apreciar en el ejemplo mas abajo, vemos que existen trazas donde el Logger recibe e imprime prematuramente un "received" antes que su respectivo "sending". Por lo tanto, podemos decir que existen interleavings donde algunos Workers esperan mucho más tiempo (tienen un Jilter muy grande) para logear el evento de sending que el Worker que lo recibe, este último, le envía al logger el mensaje de receive y se imprime con éxito.

Vale aclarar que si bien con cualquier Sleep y Jilter arbitrario se darán estas trazas. Entendemos que el tiempo que se le da a los Workers para esperar un mensaje de otro y el tiempo del cual cada Worker espera para notificarle al logger, influencian en gran medida a la posibilidad de imprimir logs fuera de orden.

> - log: na ringo {received,{hello,57}}
- log: na john {sending,{hello,57}}
- log: na john {received,{hello,77}
- log: na paul {sending,{hello,68}}

Cuando ejecutamos el método test:run(), los mensajes mostrados no están ordenados. Se sabe que están fuera de orden porque algunos mensajes "received" se registran antes de un mensaje "sending", además podemos verficiar por el valor del mensaje que tiene cada log.

> - log: na ringo {received,{hello,57}}		// Se recibe el mensaje antes de que se envíe
- log: na john {sending,{hello,57}}			// Mensaje de envío
- log: na john {received,{hello,77}}
> - log: na paul {sending,{hello,68}}

Esto sucede debido a que el proceso Logger no tiene un procedimiento para ordenar mensajes. Cuando el Loger recibe el mensaje, este lo registra inmediatamente.
También sucede lo anterior por el Jitter ya que este introduce un retardo entre envíar el mensaje al par y en informar al Loger.


### 4 Tiempo Lamport

#### 1. ¿Cómo identificamos mensajes que estén en orden incorrecto?
Ahora, los Workers llevan cuenta su tiempo lógico y que cada vez que uno le envía un mensaje a otro, el último actualiza su Time y continua trabajando. Como el tiempo lógico de cada Worker, en cada operación es enviado dentro del mensaje al logger, este lo imprime como parte del log, por lo tanto, vemos en la salida, trazas incorrectas que antes no podíamos identificar ya que no sabíamos en que tiempo estaban sucediendo.

Para ver un claro ejemplo de estas posibles trazas mostramos el siguiente interleaving:

> - log: 0 ringo {received,{hello,57}}
- log: 0 john {sending,{hello,57}}
- log: 1 john {received,{hello,77}
- log: 1 paul {sending,{hello,68}}
- log: 2 ble {sending,{hello,90}}
- log: 0 bla {sending,{hello,07}}


#### 2. ¿Qué es siempre verdadero y qué es a veces verdadero?
Lo que podemos decir es que a pesar de que cada Worker mantiene su tiempo lógico, no es verdadero afirmar que el Logger en su tarea de imprimir los mensajes que recibe, lo hará en el orden que el tiempo dice. Como vimos en el ejemplo anterior, en algunos casos se imprimirán eventos donde el tiempo se distingue notablemente del tiempo del log inmediatamente anterior.

Lo que si es cierto es que el Timer que contiene cada Worker está directamente sincronizado con el Worker que está trabajando, claro, estos cambian constantemente ya que se utiliza una selección random para el siguiente envío de mensaje. Aun así, la utilización de un Timer sincronizado entre pares no ayuda en medida alguna al loggeo de mensajes en el orden correcto.


#### 3. ¿Cómo lo hacemos seguro?



### 4.1 La parte delicada

#### 1. Debemos de alguna manera mantener una cola de retención de mensajes que no podemos entregar aun porque no sabemos si recibiremos un mensaje con un timestamp anterior. ¿Cómo sabemos si los mensajes son seguros de imprimir?




### 4.2 En el curso

#### 1. Describir si encontraron entradas fuera de orden en la primera implementación y en caso afirmativo, cómo fueron detectadas.
Si se muestran algunas entradas fuera de orden, no son muchas pero vemos que a veces llega primero el "received" antes del "sending", como vemos en lo siguiente:
> - log: 3 john{sending,{hello,84}}
- log: 4 paul {received,{hello,7}}
- log: 4 george {received,{hello,84}}
- log: 4 john {sending,{hello,7}}


#### 2. ¿Qué es lo que el log final nos muestra?
Muestra stop, esto es porque ya no hay un envío de mensajes entre los Workers, entonces el Logger ya no muestra ningún mensaje.


#### 3. ¿Los eventos ocurrieron en el mismo orden en que son presentados en el log?



#### 4. ¿Que tan larga será la cola de retención?
El límite del tamaño dependería del hardware del equipo donde se está implementado, la cola puede retener cientos de mensajes de los Workers.



### 4.3 Vectores de relojes

#### 1. ¿Qué diferencias habrían si se hubieran utilizado vectores de relojes?
