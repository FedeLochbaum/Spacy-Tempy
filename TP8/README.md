# TP8 Muty: Lock de exclusión mutua distribuido

Es necesario mencionar que en este documento se hará refencia a los siguientes conceptos:

- EM1: A lo sumo un proceso puede estar ejecutándose cada vez en la SC.
- EM2: Las peticiones para entrar y salir de la SC al final son concedidas
- EM3: Si una petición para entrar en la SC ocurrió antes que otra, entonces la entrada de la sección critica se garantiza en ese orden.



### 1.2. Los locks

#### 1. ¿Sería posible usar la cola de mensajes Erlang y dejar que los mensajes se encolen hasta que se libere el lock?
Si, se agregarían los mensajes por orden cuando el Worker hace la solicitud al Lock de "Take". Cuando en el Worker se haya terminado su tiempo deadlock se tendrá que sacar de la cola y así cuando haga otra solicitud se agregará hasta el final. Ya cuando el Lock se libere accederá al Worker que está al inicio de la cola con su Lock correspondiente a la sección crítica.

#### 2. La razón por la que lo implementamos de esta manera es para hacer explícito que los mensajes son tratados aún en el estado held.¿Por qué no estamos esperando mensajes ok?
Porque el Lock espera a que se libere la sección crítica y así poder permitir que otro Worker haga uso de ella, solo en el estado Held se manda mensaje de que ya se liberó o que siga en estado Held, esto permite que no entre más de 1 Worker a la sección crítica.



### 1.3. Algo de testing

#### 1. Seremos fácilmente capaces de testear diferentes locks con diferentes parámetros Sleep y Work. ¿Funciona bien?
El Worker al iniciar está en un estado Open, después del un tiempo random con su Sleep hará un Take al Lock, en caso de que esté ocupado por otro Worker, este mismo estará esperando en tomar el Lock, cuanto más tiempo tenga de Work (Trabajo) y menos de Sleep, el Worker la mayor parte del tiempo estará esperando, esto mismo generara tarde o temprano deadlocks entre los locks.
En lo contrario si se usa un tiempo mayor de Sleep y menos de Work, la mayor parte del tiempo el Worker estará en estado Open y muy pocas veces en estado de espera (Waiting) esto es debido al "Wait = random:uniform(Sleep)" que espera un tiempo aleatorio del Sleep y como es muy grande el tiempo de Sleep sucede que no pide muy rápido el Take del Lock y no por consecuencia no hay deadlock entre los Locks.

#### 2. ¿Qué ocurre cuando incrementamos el riesgo de un conflicto de lock? ¿Por qué?
Si esto ocurre, algunos locks podrían quedarse lockeados esperando la respuesta de otro lock que a su vez sigue esperando la respuesta de otro y esto claramente crearía una situación de deadlock entre los diferentes Locks. Una de las propiedades que debe cumplir un algoritmo de exclusión mutua es EM2, la cual dice que siempre debe concederse todas las peticiones, tarde o temprano. Por lo tanto, esta implementación no cumpliría con esta regla.



### 2. Resolviendo el deadlock

#### 1. ¿Podemos garantizar que tenemos un solo proceso en la sección crítica en todo momento?
Claramente no, si bien esta dedición permite que no ocurra deadlocks entre los locks, es decir, siempre va a poder entrar alguien en la sección critica, se puede ver que cuando un Lock que actualmente esta en la Sección critica es liberado, debe notificarle a cada uno de los Locks en espera, como en este caso es muy probable que dos locks diferentes estén esperando a este último para entrar. Cuando el lock saliente les envía el mensaje Ok, ambos locks entraran en la sección critica al mismo tiempo. Por lo tanto, de esta manera, no estaría cumpliendo con la propiedad EM1 de los algoritmos de exclusión mutua, la cual dice que solo uno puede estar en la sección critica al mismo tiempo. Aun así, es apreciable ver que con el simple hecho de darle una prioridad a los locks estos aliviaran la congestión en el tiempo de espera.

Finalmente, luego de varias pruebas, análisis de posibles trazas y vueltas al algoritmo se encontró una solución al problema de la doble entrada a la sección critica. Como un lock en estado waiting puede responderle a otro de mayor prioridad y esto generaría una posible doble entrada, vemos que si al mismo que el primer lock le envía ok, también le envía request genera una dependencia de entrada en el primer lock. Esta solución permite que antes de que dos locks queden liberados para entrar a la sección critica, uno de ellos este atado a la respuesta del de mayor prioridad, impidiendo así la doble entrada. Cuando el de mayor prioridad salga de la sección critica, le notificara al segundo y este podrá entrar sin problemas.

#### 2. ¿Qué tan eficientemente lo hace y cuál es la desventaja?
Es eficiente porque si da preferencia al Worker que tiene un identificador más alto.
Su desventaja es que como mencionamos anteriormente puede ser que 2 o más Locks pueden entrar a la sección crítica y eso no es lo correcto. Aun así, la desventaja más grande que tiene esta alternativa es la alta contención entre los locks para entrar a la sección critica.

Utilizando la alternativa propuesta en el punto anterior, vemos que además, si se manejan muchos Locks para una misma disputa de sección critica, la espera de los locks será de un tiempo mucho mayor, ya que los locks de menor prioridad siempre estarán ligados a la respuesta del de mayor prioridad para entrar a la SC.



### 3. Tiempo de Lamport

#### 1. ¿Puede darse la situación en que un worker no se le da prioridad al lock a pesar de que envió el request a su lock con un tiempo lógico anterior al worker que lo consiguió?

Realizando varias pruebas, con diferentes tiempos en los tests y verificando varias posibles trazas en el flujo del algoritmo podemos decir que no es posible que un worker con un tiempo lógico mayor ingrese antes que el worker con tiempo menor. Esto es así, ya que en esta alternativa de Lock, mientras un Lock esta en estado waiting por más que su Time sea mayor siempre le dará paso a aquel con tiempo menor y finalmente si él Time de ambos es idéntico se diferencia por la prioridad que tiene cada Lock en su creación.

Es interesante nuevamente prestarle atención a la importancia en la utilización de un Reloj lógico o Reloj de Lamport para manejar el orden de los sucesos y así evitar problemas que se dan cuando no se puede tener un reloj físico sincronico.


### 4. El reporte

Desde la opinión personal, este trabajo contó con una dificultad diferente a la provista por anteriores trabajos. Si bien en el capítulo del libro correspondiente se explican los factores de importancia y proporciona algunos algoritmos similares, no eran totalmente aplicables a nuestro diseño, fue por esto mismo que requirió comprender, analizar y estudiar posibles casos donde el algoritmo implementado era factible o no.

Comenzando con el Lock2, vemos que es necesario brindar algún orden para proporcionar la entrada a la SC, a pesar de esto, también es cierto que, como se mencionó en la sección 2, varios locks podrían estar esperando a la respuesta de un solo lock para entrar mutuamente a la SC. Como esto era un problema se propuso generar una atadura sobre el nodo de menor con el de mayor prioridad, es decir, si un nodo de menor prioridad en estado de esperar le envía Ok al de mayor prioridad se le enviara también un request ligando la entrada a la sección critica del proceso de menor prioridad a la respuesta del de mayor prioridad. Justamente como es el de mayor prioridad no le enviara ok en estado de espera, pero si cuando salga de la SC.

Con esta solución nos encontramos con un bajo indice de deadlocks, si bien esto no debería suceder, el número de deadlocks por tiempo es realmente bajo y no de mayor importancia. Aun así creemos que este número podría ser nulo eventualmente con algún nuevo ajuste.

Para finalizar con la implementación del Lock2, vimos una relación entre el número de deadklocks y la cantidad de oks que se pueden enviar en estado de espera. Por lo tanto, implementación una nueva solución donde no se verifica que IdL1 < IdActual sino que IdL1 +1 == IdActual, es decir que un proceso en estado de espera solo le responderá al inmediatamente anterior. Vimos que el número de deadlocks bajo drásticamente, hasta casi ser nulo.

Con respecto al Lock3, está claro que era necesario brindar otro tipo de orden/prioridad que solamente un número. Utilizar un id para planificar la prioridad de los procesos es realmente una mala dedición ya que siempre sucederá que algunos locks tendrán más prioridad que otros. Utilizando los Relojes Lógicos o Relojes de Lamport vemos que el orden en que se accede a la sección critica depende del tiempo lógico en el que ocurrió el pedido de acceso. Es de esperar que con esta implementación el acceso a la sección critica no solo este restringido, es decir, que cumpla con EM1 (léase más arriba), sino también que el acceso a la SC por locks esta balanceado. Por otro lado, en caso en que un lock haga un request con mismo tiempo lógico que el del lock actual en espera se le enviara un ok solo si el primero tiene prioridad por Id. Aunque utilicemos el id para desempatar en este caso, el valor por el cual se va a permitir enviarle ok será por el tiempo lógico.

Tiempo Lamport : Si T(e1) < T(e2) => e1 sucedió antes que e2.

Realizando las mismas pruebas que en Lock1 y Lock2, podemos apreciar que, a diferencia de estos últimos, el nivel de contención de los locks bajo significativamente. Además, se cumple EM1, EM2 y EM3 para todos los casos, brindando así, todas las características necesarias que debe tener un algoritmo de exclusión mutua distribuido. De todas formas realizando un test con tiempo de Sleep de 1000 ms y tiempo de Work de 2000 ms por 5 minutos continuos, vemos que se da en solo dos ocasiones la existencia de deadlock. Esto puede estar surgiendo, 1) por el bajo tiempo limite que se utiliza para denotar un deadlock o 2) algún posible caso borde no cubierto por nuestra implementación. Esto no quiere decir que el algoritmo no cumpla con las 3 propiedades de exclusión mutua, aun así seria interesante resolver este pequeño inconveniente para asegurar la ausencia de deadlock.


### Nota
Las respuesta escritas en este informe fueron verificadas con el capítulo del libro correspondiente al trabajo (DISTRIBUTED SYSTEMS Concepts and Design Fifth Edition - 17.5 Distributed).
