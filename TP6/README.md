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

Esto sucede ya que como afirma Lamport si un proceso P mantiene un reloj lógico local L y P envía un mensaje a J, pasándole L, el tiempo lógico de J dependerá de L cuando reciba el mensaje. Claro, esto genera una consecuencia lógica de orden entre dos procesos, pero no puede afirmar (de hecho este es el inconveniente) que entre diferentes procesos, esta causalidad mantenga el orden de los mensajes.


#### 2. ¿Qué es siempre verdadero y qué es a veces verdadero?
Lo que podemos decir es que a pesar de que cada Worker mantiene su tiempo lógico, no es verdadero afirmar que el Logger en su tarea de imprimir los mensajes que recibe, lo hará en el orden que el tiempo dice. Como vimos en el ejemplo anterior, en algunos casos se imprimirán eventos donde el tiempo se distingue notablemente del tiempo del log inmediatamente anterior.

Lo que si es cierto es que el Timer que contiene cada Worker está directamente sincronizado con el Worker que está trabajando, claro, estos cambian constantemente ya que se utiliza una selección random para el siguiente envío de mensaje. Aun así, la utilización de un Timer sincronizado entre pares no ayuda en medida alguna al loggeo de mensajes en el orden correcto.

Volviendo a hacer referencia sobre Lamport, decimos que si P envía un mensaje a J, la consecuencia que se da por esta causalidad es que el tiempo lógico en que se envió el mensaje es estrictamente menor al tiempo lógico en el que se recibió el mismo. Pero no podemos afirmar el caso inverso (no es bidireccional), es decir, si el tiempo lógico en que el proceso P envía un mensaje es menor al tiempo lógico en que el proceso J recibe otro mensaje, es imposible afirmar que P le envía un mensaje a J.

Nótese que en el capítulo del libro referido a este trabajo, la relación entre procesos se denota P -> J.


#### 3. ¿Cómo lo hacemos seguro?
Como dice en la sección del libro, si E es un evento que ocurre en el proceso Pi, con tiempo Ti y E' es un evento que ocurre en el proceso Pj con tiempo Tj. Definimos que el evento E con tiempo Ti sucedió primero si y solo si Ti < Tj o Ti = Tj y en caso de que los identificadores de los procesos tengan cierto orden, podríamos agregar a la condición que i < j.

Es decir, si un evento I ocurre en tiempo Ti es seguro de imprimir si y solo si, para todos los eventos vemos que Ti <= Te, donde Te es el tiempo lógico de cada evento.


### 4.1 La parte delicada

#### 1. Debemos de alguna manera mantener una cola de retención de mensajes que no podemos entregar aun porque no sabemos si recibiremos un mensaje con un timestamp anterior. ¿Cómo sabemos si los mensajes son seguros de imprimir?

Sabemos que el clock contiene información del último Timelapse de cada proceso. Gracias a esto podemos asumir que si un proceso envía un mensaje en un tiempo Ti, tal mensaje será seguro si y solo si, Ti es menor o igual a cada uno de los Timelapse del clock. Es decir, si Ti es menor o igual a cada uno de los últimos Timelapse entonces todos los procesos ya han enviado algún mensaje con un Timelapse superior, por lo cual, como Ti ya no es un mensaje prematuro este es considerado seguro y se puede imprimir.


### 4.2 En el curso

#### 1. Describir si encontraron entradas fuera de orden en la primera implementación y en caso afirmativo, cómo fueron detectadas.
Como nombramos al principio, si bien en la primer implementación se hace provecho de la causalidad que genera enviar y recibir un mensaje (enviar un mensaje siempre sucede antes que recibirlo).
Vimos que esto no era consistente cuando se trataban de diferentes procesos, es decir, que Pi||Pj. Por lo tanto, como no podemos afirmar que Pi -> Pj, existen trazas donde el tiempo no está sincronizado entre estos.

Un claro ejemplo de esto, fue el que vimos al comienzo del documento, donde todos los mensajes entre pares de Workers eran sincronicos pero entre diferentes pares no lo eran.


#### 2. ¿Qué es lo que el log final nos muestra?
Al utilizar una cola de espera para retener mensajes que aún no son seguros de imprimir, nos permite mantener en un estado de espera a los mensajes cuyos Timelapse aún no son seguros. Aun así, vemos que si bien el orden en que se imprimen los mensajes están bastante sincronizados, existen casos donde se imprime un mensaje con Timelapse menor al mensaje inmediatamente anterior.


#### 3. ¿Los eventos ocurrieron en el mismo orden en que son presentados en el log?
Claramente no, si bien esto es difícil de distinguir, habría que generar un test donde se pueda visualizar el orden en que se enviaron los mensajes para compararlo con el orden en que se imprimen. Si bien no se alcanzó con este objetivo, se puede apreciar a simple vista que, a diferencia del orden resultante en las primeras implementaciones, esta última garantiza que para poder imprimir un mensaje cualquiera, es necesario encolarlo a la cola de retención y hacerlo esperar hasta que sea seguro.
Una forma sencilla de comprobar que efectivamente está ocurriendo, es observando que no se imprime ningún mensaje con un tiempo mayor antes de terminar de imprimir otro con un tiempo menor.

Para poder apreciar esto veámoslo con un ejemplo:

>- log: 126 paul {received,{hello,98}}
- log: 127 paul {received,{hello,5}}
- log: 127 paul {sending,{hello,48}}
- log: 128 george {sending,{hello,68}}
- log: 128 george {received,{hello,48}}
- log: 129 john {sending,{hello,69}}
- log: 129 paul {received,{hello,3}}
- log: 129 john {received,{hello,68}}
- log: 129 paul {sending,{hello,9}}
- log: 129 george {sending,{hello,65}}



#### 4. ¿Que tan larga será la cola de retención?
Asumiendo que nuestra implementación de 'safe' es correcta y que garantiza que un mensaje en un tiempo T es seguro si y solo si T es menor o igual al último Timelapse de cada Worker.
Podemos decir que en el peor de los casos el tamaño de la cola de retención será de N, donde N es la cantidad de Workers trabajando. Como la cola de retención guardara mensajes cuyo Times son mayores al menor de todos dentro del clock y solo se imprimirán cada uno de ellos cuando aumente este último, afirmamos que jamás un mensaje puede esperar por más de N mensajes nuevos de Workers.


### 4.3 Vectores de relojes

#### 1. ¿Qué diferencias habrían si se hubieran utilizado vectores de relojes?

Como vimos cuando utilizamos la implementación propuesta por Lamport sucede que L(e) > L(e') no implica que e -> e'. Por esta razón surge la idea de utilizar un Vector de N enteros por cada proceso (Worker), el cual marca dicho Vector con el tiempo de cada evento. Cada Vector es inicializado con tiempo = 0 en cada una de sus posiciones. Cuando un proceso enviar un mensaje, primero actualiza su Vector incrementando el tiempo del evento y luego envía su Vector al proceso receptor. Si nuestro proceso con un vector Vi recibe un mensaje de otro proceso con un vector Vj, setea en cada una de sus posiciones Vi[e] el tiempo máximo entre Vi[e] y Vj[e]. Esto nos permite estar sincronizados con el vector de otro proceso, por lo cual, si tal vector cambio en algún momento antes de recibir el mensaje no supondrá un problema, dado que la referencia a tal vector siempre estará actualizada.

Cabe destacar que en el capítulo del libro referente al trabajo, se demuestra por inducción sobre la cantidad de sucesos que relacione los eventos e -> e' implica que V(e) < V(e') y viceversa.

Por otro lado, utilizar Vectores de relojes supone una desventaja, a diferencia de Lamport, precisa una cantidad de almacenamiento mucho mayor. Como dijimos, cada Worker trabaja con un Vector de tamaño N, donde N = cantidad de procesos.
