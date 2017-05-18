#R-Tree: Acceso a espacios multidimensionales

Para consultar a un determinado espacio es necesario conocer su ubicación (lugar geográfico). Para responder de forma eficiente hay que acceder  a un espacio donde se mantienen puntos de referencia para cierta posición, para almacenar los objetos se utilizará una estructura de datos de tipo árbol, llamado R-Tree.

Un ejemplo en el uso del proyecto sería: encontrar la ruta más óptima en una ubicación dado un punto de referencia actual, así con esto poder encontrar el camino más eficiente.

Con esto es posible realizar consultar, ya sean encontrando:
	- ciertos lugares en una cierta región
	- por intervalos de región
	- una uicación específica
	- o una trayectoria

R-Tree es un árbol balanceado donde se almacena su MBR (Minimum Bounding Rectangle), es decir, el menor rectánuglo que contiene al objeto en cuestión. Cada nodo en el R-Tree corresponde al MBR que contiene a sus hijos. Los nodos que son objetos contienen punteros a los objetos (hijos) y a la vez cada nodo se almacena en una página.