# Spacy-Tempy una Base de datos espacio-temporal distribuida

#### Propuesta: [Propuesta de trabajo final](/Propuesta.md)

## Introducción

En este proyecto se decidió Implementar una base de datos espacio temporal, utilizando una tipo de datos abstracto propuesto por el paper ["I+3 R-Tree: un método de acceso espacio-temporal"](http://sedici.unlp.edu.ar/bitstream/handle/10915/21205/Documento_completo.pdf?sequence=1) llamado I3Rtree capaz de almacenar información espacial  de aquellos nodos que se van moviendo en el espacio-tiempo. Permitiendo así, resolver consultas sobre posición, trayectoria, estadía y puntos por donde se irán moviendo estos últimos en una región determinada.
Además se incorporo los conceptos aprendidos en esta materia sobre manejo de información de manera distribuida entre diferentes nodos/computadoras, disminuyendo la carga total en cada uno de los servidores.

Se implemento a menor escalar la tarea de distribución de carga entre diferentes servidores como así la idea de agregar dinámicamente nuevas computadoras a la red del proyecto. Finalmente permitimos a nuestra red tener tolerancia a fallos, en cuales la red resiste a la caída de diferentes computadoras dentro de la red.  

Entraremos en mas detalle sobre los conceptos tratados en este proyecto, contando los detalles de la implementación a medida que avance el documento.

## I+3 R-Tree: un método de acceso espacio-temporal
Como se menciono anteriormente, la idea central de nuestro proyecto se basa en la propuesta de este paper, el cual desarrolla una nueva estructura de datos  llamada I+3 R-Tree.
I+3 R-Tree es una estructura de datos que surge de convinar un [Rtree](https://es.wikipedia.org/wiki/%C3%81rbol-R), una [LinkedList](https://en.wikipedia.org/wiki/Linked_list) y un [Map](https://jarroba.com/map-en-java-con-ejemplos/) capaces de almacenar la información  pasada y presente de todos aquellos nodos dentro de una región cualquiera.
A continuación se explicara brevemente porque es necesaria cada una de ellas y cuales son sus ventajas/desventajas.

### Rtree
El I+3 R-Tree utiliza un Rtree de 3 dimensiones para almacenar toda la información pasada de los nodos que se movieron dentro de un espacio físico.

### LinkedList
El I+3 R-Tree utiliza una LinkedList, enlazando los últimos movimientos de todos los nodos. Es necesario mantener este invariante de representación para hacer de manera eficiente la resolución de consultas espacio-temporales.

### Map
El I+3 R-Tree utiliza un Map de longitud N (donde N es la cantidad de nodos moviéndose dentro de la región), el cual permite almacenar toda la información presente de los nodos en una región.

El paper continua explicando de manera detallada cual es la diferencia entre este [TAD](https://en.wikipedia.org/wiki/Abstract_data_type) y 2+3 R
-Tree. Comenta que este ultimo solo puede resolver 2 tipos de consultas espacio-temporales y que, el I3Rtree puede resolver hasta 5 consultas.

Las consultas espacio temporal que permiten esta implementación son:

 * Identificar todos los nodos que están presentes en una región R en un instante I.

 * Identificar todos los nodos de una región R en un intervalo de tiempo [ti; tk].

 * Identificar todos los nodos que pasaron por una región especifica R.

 * Mostrar el camino que realizo un nodo N.

 * Identificar la posición actual de un nodo N.

Si bien el paper explica como llevar a cabo estas consultas, no creemos necesario entrar en detalle sobre como se resuleven de manera eficiente la busqueda de informacion espacio-temporal. Aun asi, para observar el costo de las mismas se puede ver la siguiente cita, la cual compara su eficiencia con el anteriormente mencionado 2+3 R-Tree:

#### Consulta Timeslice:
Ambas estructuras se comportan en este caso de mane
ra similar, siendo un poco mas alto  el  costo  de  consulta  en  el  2+3  R-Tree  debido  a los  accesos  a  disco  extras
necesarios para obtener las posiciones actuales de
los objetos.  

#### Consulta Intervalo
Resulta altamente similar a la eficiencia del 2+3  R-Tree.

#### Consulta Eventos
Es un subconjunto de Timeslice, el cual contiene so
lamente los objetos que ingresan o
salen  de  la  región  de  consulta  en  el  instante  de  ti
empo  dado.  Por  esta  razón,  la
evaluación  experimental  realizada  para  la  consulta
de  Timeslice  es  valida  también
para la consulta de tipo Eventos.

#### Consultas de Trayectoria
Para poder comparar el desempeño de la consulta de
trayectoria se realizó un análisis
de costo de búsqueda para las dos estructuras, tant
o el I+3 R-Tree como para el 2+3
R-Tree.  Este  análisis  de  costo  se  realizó  debido  a
que  el  2+3  R-Tree  no  posee  un
método para resolver la trayectoria.


#### Consulta Posicion Actual
Como el I+3 R-Tree utiliza un Map con el ultimo movimiento de cada nodo, el orden de la respuesta de esta consulta es el orden provisto por la busqueda del Map.

### Implementación
Nuestra implementación del I+3 R-Tree, consiste en una tupla de 3 elementos {3DRtree, Map, LinkedList}, la cual en conjunto almacena la información espacio-temporal de todos los nodos dentro de una región limitada. Un I+3 R-Tree en principio no tiene limites de espacio para manejar información espacial. Además permite a una entidad, subscribir se, desubscribirse del I+3 R-Tree y así mantener solo la información pasada de este. Permite resolver los cinco tipos de consultas espacio-temporal propuestos anteriormente manteniendo todos los invariantes de representación necesarios para que estas sean eficientes como dice el paper.

El principal problema que tuvo nuestro grupo de trabajo fue el encontrarse sin ningún apoyo de implementación por parte del paper, por lo cual recurrimos a la minuciosa lectura de los invariantes de representación y propiedades de la estructura de datos para diseñar una implementación propia. A pesar de esto, pudimos encontrar una implementación de Rtree publica en erlang que finalmente utilizamos para simplificar notablemente el tiempo dedicado a representar el I+3 R-Tree.

Como tarea bonus, se implemento la opción de partir un I+3 R-Tree en dos subárboles, cada quien con su región inicio y región final,  pero finalmente descartado para su utilización en el proyecto.

Nuevamente, no vale la pena entrar en detalles de implementación en este documento ya que si bien esta parte es muy necesaria, no es la mas interesante para resaltar en este informe.

## Servidor de base de datos secuencial utilizando I+3 R-Tree
Una vez implementada la base de datos dimensional y definida su interfaz, se decidió implementar un servidor secuencial capaz de utilizar la misma para proveer respuestas a clientes en tiempo real. Este servidor fue la primer versión que vio el proyecto, capaz de atender request de subscripción, desubscripcion y movimiento de  nodos a través del tiempo. También, capaz de respoder las cinco consultas espacio-temporal de manera concurrente.

En principio, como el servidor es secuencial decidimos que solo tenga un I+3 R-Tree el cual contenga toda la información de todos los nodos, sin limite de espacio. Esta misma se ira actualizando a medida que el server vaya respondiendo sus request a aquellas entidades  subscritas. Una vez que el servidor deja de estar subscrito, aunque este envié request de movimiento al servidor, el servidor no hará que su I+3 R-Tree handlee el pedido.

Como el objetivo de esta implementación era la de realizar una pequeña prueba de uso dinámico del I+3 R-Tree (la cual fue muy satisfactoria), fue claramente descartada para dar paso a su versión mejorada, un servidor completamente distribuido.


## Servidor distribuido utilizando I+3 R-Tree

### Arquitectura distribuida

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
