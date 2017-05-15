# TP6 Loggy: Un logger de tiempo lógico

### 3 El Test

#### 1. Corramos algunos tests y tratemos de encontrar mensajes de log que sean mostrados fuera de orden. ¿Cómo sabemos que fueron impresos fuera de orden?

Cuando ejecutamos el método test:run(), los mensajes mostrados no están ordenados. Se sabe que están fuera de orden porque algunos mensajes "received" se registran antes de un mensaje "sending", además podemos verficiar por el valor del mensaje que tiene cada log.

> - log: na ringo {received,{hello,57}}		// Se recibe el mensaje antes de que se envíe
- log: na john {sending,{hello,57}}			// Mensaje de envío
- log: na john {received,{hello,77}}
> - log: na paul {sending,{hello,68}}

Esto sucede debido a que el proceso Logger no tiene un procedimiento para ordenar mensajes. Cuando el Loger recibe el mensaje, este lo registra inmediatamente.
También sucede lo anterior por el Jitter ya que este introduce un retardo entre envíar el mensaje al par y en informar al Loger.


### 4 Tiempo Lamport

#### 1. ¿Cómo identificamos mensajes que estén en orden incorrecto? 

Por el contador del Worker, los mensajes siguen en orden equivocado como el apartado anterior (3 El Test) vemos que un Worker ya está recibiendo un mensaje antes de que se lo manden.


#### 2. ¿Qué es siempre verdadero y qué es a veces verdadero?
Siempre es verdadero el orden del contador de cada Worker, se incrementa correctamente.
A veces es verdadero que se envíe primero el log de envío y luego esté el log de recibir.


#### 3. ¿Cómo lo hacemos seguro?
Al agregar un tiempo lógco dentro de los Workers nos aseguramos de que se ordenen los mensajes.



### 4.1 La parte delicada

#### 1. Debemos de alguna manera mantener una cola de retención de mensajes que no podemos entregar aun porque no sabemos si recibiremos un mensaje con un timestamp anterior.¿Cómo sabemos si los mensajes son seguros de imprimir?

Cada Worker tiene su lista de mensajes y así mismo estos contienen un Timestamp lógico y el contenido del mensaje. Cuando se recibe un mensaje se guarda en la lista asociada al Worker que envió el mensaje.



### 4.2 En el curso

#### 1. Describir si encontraron entradas fuera de orden en la primera implementación y en caso afirmativo, cómo fueron detectadas. 



#### 2. ¿Qué es lo que el log final nos muestra? 



#### 3. ¿Los eventos ocurrieron en el mismo orden en que son presentados en el log? 



#### 4. ¿Que tan larga será la cola de retención?




### 4.3 Vectores de relojes

#### 1. ¿Qué diferencias habrían si se hubieran utilizado vectores de relojes?
