# TP7 Toty: multicast con orden total

### 2.3. Experimento

#### 1. Analizar que pasa si tenemos un sistema que se basa en una entrega con orden total pero esto no fue claramente establecido. ¿Si la congestión es baja y no tenemos retrasos en la red, cuánto tiempo tarda antes de que los mensajes se entreguen fuera de orden? 
Como ejemplo en el libro vemos que el efecto de pérdida de mensajes y la inconsitencia de ordenamiento puede depender del método de replicación y la importancia de que todas las réplicas estén actualizadas.
El ancho de banda consumido, es proporcional número de mensajes enviados en cada operación de entrada y salida.

Entonces podemos decir que entre más mensajes se estén enviando los Workers, el Cast incrementará su capacidad para almacenar y distribuir los datos a cada uno. El tiempo ha estimar también dependería del tiempo que tiene cada Worker en el envío de sus mensajes, entre más corto el tiempo, el Cast se congestionará de mensajes y este podría enviar mensajes fuera de orden.


#### 2. ¿Cuán difícil es hacer debug del sistema y darnos cuenta que es lo que está mal?
Para checar si funciona correctamente, lo que hicimos fue incrementar el número de Workers y así podemos ver si los mensajes se entregan a tiempo y en orden para cada uno de los Workers.
Lo que hicimos para mostrar donde estamos mal fueron hacer pruebas, cambiando parámteros de Sleep de los Worker, el número de Workers, el Jitter para el envío de mensajes del Cast.
	


### 4. Experimentos

#### 1. Probar usando el multicaster de orden total. ¿Mantiene los workers sincronizados?
Si los mantiene sincronizados, hay integridad en los envíos de mensajes esto es porque los mensajes se mantienen en orden en que hacen la petición para el cast, y estos se van liberando uno a uno para todos los Workers.

Los colores que muestran las ventanas al correr el test son iguales, la implementacion del módulo multicast con orden total nos garantiza que todos los mensajes de multicast están ordenados de acuerdo a los workers.


#### 2. Tenemos muchos mensajes en el sistema, ¿Cuántos mensajes podemos hacer multicast por segundo y cómo depende esto del número de workers?
Cuando un proceso Worker hace un multicast de un mensaje, este lo libera inmediatamente porque no hay más mensajes para enviar, pero si 2 o más procesos Worker hacen el envío, tendrá una espera para enviarlos debido a que los mantiene en una región de espera y los enviará hasta liberar los demás.


#### 3. Construir una red distribuida, ¿cuán grande puede ser antes de que empiece a fallar?
Hay varios aspectos a considerar para que una red distribuida llegue a tener fallas, por ejemplo que falle en un proceso, la ejecución arroja un resultado incorrecto, esto puede ser por un bloqueo de procesos o un interbloqueo, tiempo expirado.
Los sistemas distribuidos tienen una gran ventaja sobre los sistemas centralizados y es porque son tolerantes a fallos. No quiere decir que nunca vaya a fallar pero que una tarea falle o en nuestro caso un worker no reciba su mensaje o no envié eso no evita que los demás workers puedan seguir funcionando correctamente.

Una red distribuida tiene capacidad de crecimiento incremental, es decir se puede añadir más procesamiento al sistema incrementando su pontencia en forma gradual según las necesidades. Entonces puede ser tan grande como sea necesario, claramente es necesario que el sistema funcione correctamente, es decir que las fallas no sean por lógica o implementación, sino que puede fallar por pérdida de mensajes, saturación en tráfico, etc.
