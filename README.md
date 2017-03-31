# TP2 Rudy: un servidor web sencillo
---

Como sabemos el modelo cliente-servidor se caracteriza por un servidor central y un número indeterminado de clientes. Se utiliza generalmente para las operaciones de gestión de recursos, donde el servidor se encarga de gestionar los recuros.

Con está práctica aprendimos un poco más acerca de la estructura y reglas del lenguaje, así también como su funcionamiento, el paso de mensajes entre las funciones y módulos.


## Preguntas
 #####  3. El ejercicio
  1. ¿Cuántos requests por segundo podemos servir? 
        Aproximadamente de 25 a 30.

  2. ¿Nuestro delay artificial es importante o desaparece dentro del overhead de parsing? 
        El delay artificial aumentado en gran medida la respuesta del servidor haciendolo mucho más lento.

*Reporte de tiempos entre cliete-servidor del módulo Test.*
| Tiempo (segundos) |
| :------: |
| 0.035 | 
| 0.040 | 
| 0.828 | 
| 0.840 | 
| 0.953 |
| 1.032 | 

 #####  4.1 Incrementando el rendimiento
  3. ¿Qué ocurre si ejecutamos los benchmarks en varias máquinas al mismo tiempo?
        Se ejecuta correctamente pero si se nota un tiempo de espera no muy adecuado, esto puede ser debido a diferentes factores como: la red en la que están conectados, el procesamiento del servidor.

  4. ¿Deberíamos crear un nuevo proceso por cada request de entrada?
        Si.

  5. ¿Toma tiempo crear un nuevo proceso? 
        No, pero al lenvantar muchos procesos la respuesta se vuelve mucho más lenta.

  6. ¿Qué ocurriría si tenemos miles de requests por
minuto?
        El servidor pone en cola de espera todos los request que están procesados.

 #####  4.2 Parseo HTTP
 7. ¿Cómo sabremos el tamaño del cuerpo?
    