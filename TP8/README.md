# TP8 Muty: Lock de exclusión mutua distribuido

### 1.2. Los locks

#### 1. ¿Sería posible usar la cola de mensajes Erlang y dejar que los mensajes se encolen hasta que se libere el lock?


#### 2. La razón por la que lo implementamos de esta manera es para hacer explícito que los mensajes son tratados aún en el estado held.¿Por qué no estamos esperando mensajes ok?
Porque el Lock espera a que se libere la sección crítica y así poder permitir que otro Worker haga uso de ella, solo en el estado Held se manda mensaje de que ya se liberó o que siga en estado Held, esto permite que no entre más de 1 Worker a la sección crítica.



### 1.3. Algo de testing

#### 1. Seremos fácilmente capaces de testear diferentes locks con diferentes parámetros Sleep y Work. ¿Funciona bien?
El Worker al iniciar está en un estado Open, después del un tiempo random con su Sleep hará un Take al Lock, en caso de que esté ocupado por otro Worker, este mismo estará esperando en tomar el Lock, entre más tiempo tenga de Work (Trabajo) y menos de Sleep, el Worker la mayor parte del tiempo estará esperando, esto mismo generara tarde o temprano deadlocks entre los locks.
En lo contrario si se usa un tiempo mayor de Sleep y menos de Work, la mayor parte del tiempo el Worker estará en estado Open y muy pocas veces en estado de espera (Waiting) esto es debido al "Wait = random:uniform(Sleep)" que espera un tiempo aleatorio del Sleep y como es muy grande el tiempo de Sleep sucede que no pide muy rápido el Take del Lock y no por consecuencia no hay deadlock entre los Locks.

#### 2. ¿Qué ocurre cuando incrementamos el riesgo de un conflicto de lock? ¿Por qué?
Si esto ocurre, algunos locks podrian quedarse lockeados esperando la respuesta de otro lock que a su vez sigue esperando la respuesta de otro y esto claramente crearia una situacion de deadlock entre los diferentes Locks. Una de las propiedades que debe cumplir un algoritmo de exclusion mutua es EM2, la cual dice que siempre debe concederse todas las peticiones, tarde o temprano. Por lo tanto esta implementacion no cumpliria con esta regla.



### 2. Resolviendo el deadlock

#### 1. ¿Podemos garantizar que tenemos un solo proceso en la sección crítica en todo momento?
Claramente no, si bien esta desicion permite que no ocurra deadlocks entre los locks, es decir, siempre va a poder entrar alguien en la seccion critica,  se puede ver que cuando un Lock que actualmente esta en la Seccion critica es liberado, debe notificarle a cada uno de los Locks en espera, como en este caso es muy probable que dos locks diferentes esten esperando a este ultimo para entrar. Cuando el lock saliente les envia el  mensaje Ok, ambos locks entraran en la seccion critica al mismo tiempo. Por lo tanto de esta manera no estaria cumpliendo con la propiedad EM1 de los algoritmos de exclusion mutua, la cual dice que solo uno puede estar en la seccion critica al mismo tiempo. Aun asi, es apreciable ver que con el simple hecho de darle una priodad a los locks estos aliviaran la congestion en el tiempo de espera.

Finalmente luego de varias pruebas, analisis de posibles trazas y vueltas al algoritmo se encontro una solucion al problema de la doble entrada a la seccion critica. Como un lock en estado waiting puede responderle a otro de mayor prioridad y esto generaria una posible doble entrada, vemos que si al mismo que el primer lock le envia ok, tambien le envia request genera una dependencia de entrada en el primer lock. Esta solucion permite que antes de que dos locks queden liberados para entrar ala seccion critica, uno de ellos este atado a la respuesta del de mayor prioridad, impidiendo asi la doble entrada. Cuando el de mayor prioridad salga de la seccion critica, le notificara al segundo y este podra entrar sin problemas.

#### 2. ¿Qué tan eficientemente lo hace y cuál es la desventaja?
Es eficiente porque si da preferencia al Worker que tiene un identificador más alto.
Su desventaja es que como mencionamos anteriormente puede ser que 2 o más Locks pueden entrar a la sección crítica y eso no es lo correcto. Aun asi, la desventaja mas grande que tiene esta alternativa es la alata contencion entre los locks para entrar a la seccion critica.

Utilizando la alternativa propuesta en el punto anteior, vemos que ademas, si se manejan muchos Locks para una misma disputa de seccion critica, la espera de los locks sera de un tiempo mucho mayor, ya que los locks de menor prioridad siempre estaran ligados a la respuesta del de mayor prioridad para entrar a la SC.



### 3. Tiempo de Lamport

#### 1. ¿Puede darse la situación en que un worker no se le da prioridad al lock a pesar de que envió el request a su lock con un tiempo lógico anterior al worker que lo consiguió?



### Nota
Las respuesta escritas en este informe fueron verificadas con el capítulo del libro correspondiente al trabajo (DISTRIBUTED SYSTEMS Concepts and Design Fifth Edition - 17.5 Distributed).
