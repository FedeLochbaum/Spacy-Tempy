# Spacy-Tempy una Base de datos espacio-temporal distribuida

#### Propuesta: [Propuesta de trabajo final](/Propuesta.md)

## Introducción

En este proyecto se decidió Implementar una base de datos espacio temporal, utilizando una tipo de datos abstracto propuesto por el paper ["I+3 R-Tree: un método de acceso espacio-temporal"](http://sedici.unlp.edu.ar/bitstream/handle/10915/21205/Documento_completo.pdf?sequence=1) llamado I3Rtree capaz de almacenar información espacial  de aquellos nodos que se van moviendo en el espacio-tiempo. Permitiendo así, resolver consultas sobre posición, trayectoria, estadía y puntos por donde se irán moviendo en una región determinada.
Además se incorporo los conceptos aprendidos en esta materia sobre manejo de información de manera distribuida entre diferentes nodos/computadoras, disminuyendo la carga total en cada uno de los servidores.

Se implemento a menor escalar la tarea de distribución de carga entre diferentes servidores como así la idea de agregar dinámicamente nuevas computadoras a la red del proyecto. Finalmente permitimos a nuestra red de servidores tener tolerancia a fallos, donde los servidores toleran  la baja de las diferentes computadoras dentro de la red distribuida.  

Entraremos en mas detalle sobre los conceptos tratados en este proyecto, contando los detalles de implementación a medida que avance el documento.

## I+3 R-Tree: un método de acceso espacio-temporal
Como se menciono anteriormente, la idea central de nuestro proyecto se basa en la propuesta de este paper, el cual desarrolla una nueva estructura de datos  llamada I+3 R-Tree.
I+3 R-Tree es una estructura de datos que surge de combinar un [Rtree](https://es.wikipedia.org/wiki/%C3%81rbol-R) de tres dimensiones (dimension x, dimension y e tiempo), una [LinkedList](https://en.wikipedia.org/wiki/Linked_list) y un [Map](https://jarroba.com/map-en-java-con-ejemplos/) capaces de almacenar la información  pasada y presente de todos aquellos nodos dentro de una región cualquiera a lo largo del tiempo.
A continuación se explicara brevemente porque es necesaria cada una de ellas y cuales son sus ventajas/desventajas.

### Rtree
El I+3 R-Tree utiliza un Rtree de 3 dimensiones para almacenar toda la información pasada de los nodos que se movieron dentro de un espacio físico e informacion adicional como, cuanto tiempo han permanecido en ese punto antes de moverse.

### LinkedList
El I+3 R-Tree utiliza una LinkedList, enlazando los últimos movimientos de todos los nodos. Es necesario mantener este invariante de representación para hacer de manera eficiente la resolución de consultas espacio-temporales.

### Map
El I+3 R-Tree utiliza un Map de longitud N (donde N es la cantidad de nodos moviéndose dentro de la región), el cual permite almacenar toda la información presente de los nodos en una región.

El paper continua explicando de manera detallada cual es la diferencia entre este [TAD](https://en.wikipedia.org/wiki/Abstract_data_type) y 2+3 R-Tree. Comenta que este ultimo solo puede resolver 2 tipos de consultas espacio-temporales y que, el I3Rtree puede resolver hasta 5 consultas.

Las consultas espacio temporal que permiten esta implementación son:

 * Identificar todos los nodos que están presentes en una región R en un instante de tiempo I.

 * Identificar todos los nodos de una región R en un intervalo de tiempo [ti; tk].

 * Identificar todos los nodos que pasaron por una región especifica R.

 * Mostrar el camino que realizo un nodo N.

 * Identificar la posición actual de un nodo N.

Si bien el paper explica como llevar a cabo estas consultas, no creemos necesario entrar en detalle sobre como se resuleven de manera eficiente la busqueda de informacion espacio-temporal. Aun asi, para observar el costo de las mismas se puede ver la siguiente cita, la cual compara su eficiencia con el anteriormente mencionado 2+3 R-Tree:

#### Consulta Timeslice:
Ambas estructuras se comportan en este caso de manera similar, siendo un poco mas alto  el  costo  de  consulta  en  el  2+3  R-Tree  debido  a los  accesos  a  disco  extras necesarios para obtener las posiciones actuales de los objetos.  

#### Consulta Intervalo
Resulta altamente similar a la eficiencia del 2+3  R-Tree.

#### Consulta Eventos
Es un subconjunto de Timeslice, el cual contiene solamente los objetos que ingresan o salen  de  la  región  de  consulta  en  el  instante de  tiempo  dado.  Por  esta  razón,  la evaluación  experimental  realizada  para  la  consulta de  Timeslice  es  valida  también para la consulta de tipo Eventos.

#### Consultas de Trayectoria
Para poder comparar el desempeño de la consulta de trayectoria se realizó un análisis de costo de búsqueda para las dos estructuras, tanto el I+3 R-Tree como para el 2+3R-Tree.  Este  análisis  de  costo  se  realizó  debido  a que  el  2+3  R-Tree  no  posee  un método para resolver la trayectoria.


#### Consulta Posicion Actual
Como el I+3 R-Tree utiliza un Map manteniendo el ultimo movimiento de cada nodo, el orden de la respuesta de esta consulta es el orden provisto por la busqueda del Map.

### Implementación
Nuestra implementación del I+3 R-Tree, consiste en una tupla de 3 elementos {3DRtree, Map, LinkedList}, la cual en conjunto almacena la información espacio-temporal de todos los nodos dentro de una región limitada. Un I+3 R-Tree en principio no tiene limites de espacio para manejar información espacial. Además permite a una entidad, subscribir se, desubscribirse del I+3 R-Tree y así mantener solo la información pasada de este. Permite resolver los cinco tipos de consultas espacio-temporal propuestos anteriormente manteniendo todos los invariantes de representación necesarios para que estas sean eficientes como dice el paper.

El principal problema que tuvo nuestro grupo de trabajo fue el encontrarse sin ningún apoyo de implementación por parte del paper, por lo cual recurrimos a la minuciosa lectura de los invariantes de representación y propiedades de la estructura de datos para diseñar una implementación propia. A pesar de esto, pudimos encontrar una implementación de Rtree publica en erlang que finalmente utilizamos para simplificar notablemente el tiempo dedicado a representar el I+3 R-Tree.

Como tarea bonus, se implemento la opción de partir un I+3 R-Tree en dos subárboles, cada quien con su región inicio y región final,  pero finalmente descartado para su utilización en el proyecto.

Nuevamente, no vale la pena entrar en detalles de implementación en este documento ya que si bien esta parte es muy necesaria, no es la mas interesante para resaltar en este informe.

## Servidor secuencial utilizando I+3 R-Tree
Una vez implementada la base de datos dimensional y definida su interfaz, se decidió implementar un servidor secuencial capaz de utilizar la misma para proveer respuestas a clientes en tiempo real. Este servidor fue la primer versión que vio el proyecto, capaz de atender request de subscripción, desubscripcion y movimiento de  nodos a través del tiempo. También, capaz de respoder las cinco consultas espacio-temporal de manera concurrente.

En principio, como el servidor es secuencial decidimos que solo tenga un I+3 R-Tree el cual contenga toda la información de todos los nodos, sin limite de espacio. Esta misma se ira actualizando a medida que el server vaya respondiendo sus request a aquellas entidades  subscritas. Una vez que el nodo deja de estar subscrito, aunque este envié request de movimiento al servidor, el servidor no hará que su I+3 R-Tree handlee el pedido.

Como el objetivo de esta implementación era la de realizar una pequeña prueba de uso dinámico del I+3 R-Tree (la cual fue muy satisfactoria), fue claramente descartada para dar paso a su versión mejorada, un servidor completamente distribuido.


## Servidor distribuido utilizando I+3 R-Tree
Luego de implementar una primera versión del servidor utilizando un I+3 R-Tree, descubrimos algunas limitaciones y propiedades que debíamos tener en cuenta para esta nueva versión. Además, buscamos simplificar notablemente la interfaz provista por el server hacia sus clientes, permitiendo solo hacer request de movimiento y consultas entre el/los servidores y la entidad en movimiento. Como el objetivo del proyecto es de crear una base de datos distribuida generalizable, decimos abstraernos totalmente de algún dominio de uso especifico.

Vimos que el mejor camino a simplificar la tarea de capturar request de entidades en movimiento dentro de una región era la de mantener no solo un servidor, sino poder tener muchos capaces de resolver los pedidos del cliente. Esta misma idea, nos genero varias preguntas interesantes como ¿ Cuantos servidores tener ? ¿ Como dividir los servidores ? ¿ Quien se encarga de responderle al cliente ? entre muchas otras. Optamos por diseñar los servidores de tal manera que trabajen en conjunto, independientemente de la cantidad de ellos. Algo interesante de mencionar, que se hará referencia cuando se explique la arquitectura de los mismos, es la de, no solo mantener distribución continua entre los servidores, sino también concurrencia en las tareas mas complejas.

Analizando un poco  uso central el proyecto, pensamos algunas alternativas para respoder las preguntas mencionadas y llegamos a la conclusión en que, por un lado, los servidores deben poder manejarse de manera transparente a la cantidad total de pares que tengan, por otro lado, nos encontramos en el dilema de como dividir la responsabilidad de cada servidor. Una de ellas es la que, un servidor se encargara de un conjunto finito de clientes, reduciendo altamente la carga de otros servidores. La otra opción es la que, un servidor se encargara de recibir request de todos los clientes que se estén moviendo en una región especifica y así, distribuir en mayor medida la carga entre todos servidores, ya que cada servidor se ocupara de una única región física.


### Arquitectura distribuida
Una vez definidos los principales problemas a atacar, diseñamos una arquitectura especifica para el manejo de los servidores distribuidos.

Decimos utilizar una arquitectura de anillo entre los servidores para delegar la responsabilidad de estos a la hora de procesar un request de un cliente, por ejemplo, si se movió de un servidor a otro (lo cual es transparente para el cliente), donde esto genera una desubscripcion de uno de los servers y una subscripción en otro.
Otra ventaja que provee esta arquitectura es que no importa a que servidor se este haciendo la consulta, si el servidor al que le llega el request se da cuenta que no debe encargarse de tal pedido, este delegara a su siguiente la responsabilidad y continuara trabajando.

Entonces, un servidor sera un proceso que puede estar corriendo, tanto en el mismo nodo local que sus pares como en diferentes, registrándose con un nombre único. Este servidor en principio tendrá un rango mínimo y máximo ({Xmin, Ymin}, {Xmax, Ymax}) del cual se encarga de procesar request de los clientes y además, conocer el rango rango físico máximo que es capaz de responder la red de servidores. Una vez creado este servidor esperara a que le notifiquen que servidores serán sus pares a la hora de procesar requests espacio-temporales y el servidor siguiente al mismo, como mencionamos anteriormente estaremos utilizando una arquitectura de anillo para simplificar notablemente el handleo de request entre servidores. Para concretar la idea, un servidor distribuido se encargara de procesar requests de una región física limitada, conocera a sus pares y además, conocera a su siguiente, entonces, cada uno de los servidores (que pueden estar corriendo en diferentes computadoras) tendrá un  I+3 R-Tree almacenando la información de esa región.

Es necesario que cada servidor conozca a cada uno de sus pares por varios motivos, uno de ellos es que, como ahora cada servidor se encargara de almacenar la información espacial de un rango limitado, para resolver las consultas espacio-temporales se requiere de la respuesta de los otros servidores, veamos un ejemplo de esto. Si un cliente hace una consulta pidiendo saber cual fue su recorrido desde un instante Ti a otro Tk, vemos que un solo servidor ahora, no es suficiente para responderle a tal cliente ya que solo conoce parte de la respuesta. Vemos tambien que es necesario almacenar la respuesta de todos sus servidores pares, ordenarlas y enviarle la respuesta final a cliente. Este es el punto mas interesante para aplicar la concurrencia en nuestro proyecto, sabemos que realizar una consulta, puede ser costosa en términos de tiempo y que además, no puede lockear temporalmente a los servidores, por lo tanto para realizar el procesamiento de consultas espacio-temporales utilizamos concurrencia todo el tiempo. Para que esto quede claro, en la siguiente sección mostraremos un ejemplo visual y se explicara mas detalladamente.

Otro motivo por el cual es necesario que cada uno de los servidores conozca a todos sus pares es que, para realizar algunas tareas que requieren consenso (que se detallaran mas adelante) como por ejemplo el balanceo de carga, control de fallos, entre otros.

Vale aclarar que, hacer que todos los servidores se conozcan entre si, abre un abanico de posibilidades para crear nueva funcionalidad que necesite gran distribución de carga.



### Manejo de Consultas distribuidas

### Balanceo automatico de carga local

### Adicion de nuevos servidores "On the fly"

### Utilizando nodos Erlang


## Managers

### Arquitectura

### Adicion de nuevos managers "On the fly"

### Control de fallos


## Pruebas

### Bench

### Limites

### Estadisticas Locales vs Distribuida


## Trabajo Futuro

### Mejor control de fallos

### Algoritmos de exclusion mutua para decidir quien particiona un server


## Conclusion

### Transparencia para el usuario
