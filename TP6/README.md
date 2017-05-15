# TP6 Loggy: Un logger de tiempo lógico

### 3 El Test

#### 1. Corramos algunos tests y tratemos de encontrar mensajes de log que sean mostrados fuera de orden. ¿Cómo sabemos que fueron impresos fuera de orden?

Como podemos apreciar en el ejemplo mas abajo, vemos que existen trazas donde el logger recibe e imprime prematuramente un "received" antes que su respectivo "sending". Por lo tanto podemos decir que existen interlivings donde algunos  Workers esperan mucho mas tiempo (tienen un Jilter muy grande) para logear el evento de sending que el Worker que lo recibe, este ultimo, le envia al logger el mensaje de receive y se imprime con exito.

Vale aclarar que si bien con cualquier Sleep y Jilter arbitrario se daran estas trazas. Entendemos que el tiempo que se le da a los Workers para esperar un mensaje de otro y el tiempo del cual cada Worker espera para notificarle al logger, influencian en gran medida a la posbilidad de imprimir logs fuera de orden.

> - log: na ringo {received,{hello,57}}
- log: na john {sending,{hello,57}}
- log: na john {received,{hello,77}
- log: na paul {sending,{hello,68}}



### 4 Tiempo Lamport

#### 1. ¿Cómo identificamos mensajes que estén en orden incorrecto?
Ahora, los Workers llevan cuenta su tiempo logico y que cada vez que uno le envia un mensaje a otro, el ultimo actualiza su Time y continua trabajando. Como el tiempo logico de cada Worker, en cada operacion es enviado dentro del mensaje al logger, este lo imprime como parte del log, por lo tanto vemos en la salida, trazas incorrectas que antes no podiamos identificar ya que no sabiamos en que tiempo estaban sucediendo.

Para ver un claro ejemplo de estas posibles trazas mostramos el siguiente interliving:

> - log: 0 ringo {received,{hello,57}}
- log: 0 john {sending,{hello,57}}
- log: 1 john {received,{hello,77}
- log: 1 paul {sending,{hello,68}}
- log: 2 ble {sending,{hello,90}}
- log: 0 bla {sending,{hello,07}}


#### 2. ¿Qué es siempre verdadero y qué es a veces verdadero?
Siempre es verdadero el orden del contador de cada Worker, se incrementa correctamente.
A veces es verdadero que se envíe primero el log de envío y luego esté el log de recibir.


#### 3. ¿Cómo lo hacemos seguro?




### 4.1 La parte delicada

#### 1. Debemos de alguna manera mantener una cola de retención de mensajes que no podemos entregar aun porque no sabemos si recibiremos un mensaje con un timestamp anterior.¿Cómo sabemos si los mensajes son seguros de imprimir?




### 4.2 En el curso

#### 1. Describir si encontraron entradas fuera de orden en la primera implementación y en caso afirmativo, cómo fueron detectadas.



#### 2. ¿Qué es lo que el log final nos muestra?



#### 3. ¿Los eventos ocurrieron en el mismo orden en que son presentados en el log?



#### 4. ¿Que tan larga será la cola de retención?




### 4.3 Vectores de relojes

1. ¿Qué diferencias habrían si se hubieran utilizado vectores de relojes?
