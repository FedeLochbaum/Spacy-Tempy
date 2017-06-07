Se desea implementar un servicio el cual utilice un I+3Rtree descripto en el paper. Este servicio debe permitir registrar cualquier tipo de nodo con su posicion inicial P en un instante T, cada vez que un nodo se desplaza este le notificara al servidor que se ha movido U unidades en el plano. El servidor, debe permitir que cualquier nodo, se registre y desregistre en cualquier momento, por otro lado, el mismo debe mantener un historial de todos los movimientos de los nodos en el plano. Es necesario que la tarea de actualizar la base de datos sea de manera distribuida (aun no sabemos como hacer esto) y concurrente.

El servidor permitira la realizacion de consultas de 4 tipos sobre la base de datos espacio-temporal.
* Debe poder identificar todos los nodos que estan presentes en una region R en un instante I.
* Debe poder identificar todos los nodos de una region R en un intervalo de tiempo [ti;tk].
* Debe poder identificar todos los nodos que pasaron por una region especifica R.
* Debe poder mostrar el camino que realizo un nodo N.

La motivacion de este proyecto es la de simular un servidor capaz de mantener sincronizados una gran cantidad de nodos en el espacio-tiempo el cual contenga un historial de todos aquellos nodos. Para esto, es necesario utilizar una estructura de datos llamada I+3Rtree propuesta por el paper, la cual simularia una base de datos espacio temporal y capas de realizar 4 diferentes metodos de accesos. Entendemos que este proyecto tiene un gran potencial y que su nivel de complejidad es muy alto.

Un ejemplo de utilizacion real es la de un servidor que este conectado a todos los autos de un espacio determinado y esté al tanto de cada uno de sus movimientos realizados por los mismos, llevando un registro espacio-temporal. De esta manera poder determinar, por ejmeplo, que clase de atajos realizan los conductores, a que velocidad se mueven, poder identificar el paradero de un auto, localizar a todos los autos en una region peligrosa o realizar algun estudio sobre la conduccion en ciertas regiones.

La estructura del I+3RTree está compuesta por un R-Tree de 3 dimensiones (3D R-Tree), para almacencar datos históricos, y la estructura denominada I para guardar datos correspondientes a las posiciones actuales de los objetos. El uso del 3D R-Tree nos permitirá recuperar información relacionada a la permanencia de los objetos en determinadas posiciones o regiones, en instantes de tiempo pasado (historia de los objetos). Mientras que con la estructura I se recupera las posiciones actuales de los objetos.

Al iniciar el uso del programa se tendrán varios servidores donde cada uno se encargará de una cantidad finita de clientes y entre ellos se conocerán, enviandose mensajes, esto con el fin de ser distribuido, mejorar la eficiencia en las peticiones de los clientes, tener la tolerancia fallos con la replicación en los datos.
El servidor también podrá ser capaz de encargarse no solo de un número finito de clientes sino también estos se dividirán por regiones del R-Tree.

Cuando los clientes realizan una petición, el servidor encargado de ese cliente o de la región en la que se encuentre el cliente, será el que procese su solicitud, dándole la repuesta o realizando la petición.