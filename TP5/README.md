# TP5 Namy: un name server distribuido

### Usando el Cache

#### 1. En la configuración simple el ttl está puesto en 0. ¿Qué sucede si cambiamos esto a 2 o 4 segundos?
Como cada servidor posee su TTL en 0, cuando el resolver resuelve un request del client, deberá recorrer siempre todo el dominio de búsqueda hasta llegar al Root. En una primera instancia la búsqueda debería ser así, ya que el resolver posee la cache vacía. Una vez cacheado el request del client (cada subdominio asignado a su server) es deseable que la cache sepa responder de manera veloz los siguientes request que consulten por un subdominio similar.

Si usáramos un TTL fijo para todos los servers tampoco seria muy inteligente. Es cierto que la tasa de fallos bajaría mucho y que el cache siempre podrá responder (hablando de un TTL grande) pero también es cierto que los request más pedidos deben ser los más cacheados y los menos pedidos eliminados.

Claramente, teniendo en cuenta solo la eficiencia para resolver un request, en cambio, si sabemos que los servers cambian su Response muy rápido, el TTL asignado para ese server deberá ser más chico, ya que el valor cacheado no estaría actualizado.

#### 2. ¿Cuánto se reduce el tráfico?
Como dijimos anteriormente, si el TTL asignado en cada server es 0, no estaríamos usando la cache, de hecho seria ineficiente. Como el TTL es 0, durante la búsqueda del valor cacheado, la cache verificara su validez y por lo tanto nunca será valido. Así deberá llegar al Root y comenzar la búsqueda como si fuese una primera vez.

Asumiendo un TTL = 2 o TTL = 4, podemos ver que la tasa de fallos sigue siendo alta, de hecho, correr el mismo request dos veces seguidas indica que todo el dominio y subdominio se cacheó con éxito pero que los valores encontrados en la cache eran invalidos.

En cambio, si ponemos un TTL = time:inf(), apreciamos que la respuesta de un segundo request siempre es exitosa, obviamente dado que no tiene limite de tiempo a la hora de mantener un valor cacheado y que por lo tanto siempre será valido.

Como conclusión decimos que usar un TTL = 2 o 4 es insignificante, debería evaluarse un TTL que no sea excesivamente grande, pero que pueda mantener un server valido aunque sea una cantidad decimal de segundos.


#### 3. Cambiarlo a un par de minutos y mover los hosts, esto es apagarlos e iniciarlos registrándolos bajo un nuevo nombre. ¿Cuándo se encuentra el nuevo server, cuantos nodos necesitan
saber sobre el cambio?


#### 4. ¿Cómo puede la cache organizarse mejor?
Para ser más eficiente es necesario mantener solo los datos que se usan frecuentemente, con esto es borrar los datos inecesarios. Para borrarlos se puede hacer que dado un cierto tiempo la información que no se ha usado en cache sea borrada y con estó se reducen los tiempos de búsqueda.


#### 5. ¿Cómo podemos reducir el tiempo de búsqueda?
Manteniendo en la cache solo los datos necesarios que se usan con más frecuencia.


#### 6. ¿Podemos usar una tabla de hash o un árbol?



---
Como sabemos la cache es una memoria auxiliar donde se almacenan datos o archivos para acceder a ellos de forma más sencilla y rápida. Es muy útil pero a veces puede causar problemas para eso se debe verificar si la cache está actualizada, también ayuda mucho que la cache solo tenga información que sea utilizada.
