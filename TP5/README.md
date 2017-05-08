# TP5 Namy: un name server distribuido

### Usando el Cache

#### 1. En la configuración simple el ttl está puesto en 0. ¿Qué sucede si cambiamos esto a 2 o 4 segundos?
Esto hace que el tiempo de vida para la búsqueda en el servidor sea mayor, haciendo un mejor rendimiento para las request de los clientes.


#### 2. ¿Cuánto se reduce el tráfico? 



#### 3. Cambiarlo a un par de minutos y mover los hosts, esto es apagarlos e iniciarlos registrándolos bajo un nuevo nombre. ¿Cuándo se encuentra el nuevo server, cuantos nodos necesitan
saber sobre el cambio?


#### 4. ¿Cómo puede la cache organizarse mejor?
Para ser más eficiente es necesario mantener solo los datos que se usan frecuentemente, con esto es borrar los datos inecesarios. Para borrarlos se puede hacer que dado un cierto tiempo la información que no se ha usado en cache sea borrada y con estó se reducen los tiempos de búsqueda.


#### 5. ¿Cómo podemos reducir el tiempo de búsqueda? 
Manteniendo en la cache solo los datos necesarios que se usan con más frecuencia.


#### 6. ¿Podemos usar una tabla de hash o un árbol?



---
Como sabemos la cache es una memoria auxiliar donde se almacenan datos o archivos para acceder a ellos de forma más sencilla y rápida. Es muy útil pero a veces puede causar problemas para eso se debe verificar si la cache está actualizada, también ayuda mucho que la cache solo tenga información que sea utilizada.