# TP8 Muty: Lock de exclusión mutua distribuido

### 1.2. Los locks

#### 1. ¿Sería posible usar la cola de mensajes Erlang y dejar que los mensajes se encolen hasta que se libere el lock?


#### 2. La razón por la que lo implementamos de esta manera es para hacer explícito que los mensajes son tratados aún en el estado held.¿Por qué no estamos esperando mensajes ok?
Porque el Lock espera a que se libere la sección crítica y así poder permitir que otro Worker haga uso de ella, solo en el estado Held se manda mensaje de que ya se liberó o que siga en estado Held, esto permite que no entre más de 1 Worker a la sección crítica.



### 1.3. Algo de testing

#### 1. Seremos fácilmente capaces de testear diferentes locks con diferentes parámetros Sleep y Work. ¿Funciona bien? 
El Worker al iniciar está en un estado Open, después del un tiempo random con su Sleep hará un Take al Lock, en caso de que esté ocupado por otro Worker, este mismo estará esperando en tomar el Lock, entre más tiempo tenga de Work (Trabajo) y menos de Sleep, el Worker la mayor parte del tiempo estará esperando, aunque a veces se rendirá y entrará en estado Open, rapidamente hará petición del Take y si sigue ocupado el Lock estará esperando hasta tomarlo.
En lo contrario si se usa un tiempo mayor de Sleep y menos de Work, la mayor parte del tiempo el Worker estará en estado Open y muy pocas veces en estado de espera (Waiting) esto es debido al "Wait = random:uniform(Sleep)" que espera un tiempo aleatorio del Sleep y como es muy grande el tiempo de Sleep sucede que no pide muy rápido el Take del Lock y no por consecuencia no hay deadlock entre los Workers.

#### 2. ¿Qué ocurre cuando incrementamos el riesgo de un conflicto de lock? ¿Por qué?




### 2. Resolviendo el deadlock

#### 1. ¿Podemos garantizar que tenemos un solo proceso en la sección crítica en todo momento?
No es 100% garantizado ya que si uno o varios Worker tienen un identificador más alto del Worker que tiene la sección crítica, el Worker que tiene uso de la sección crítica al liberarlo en Lock da acceso a otro Worker en este caso 2 Worker entran a la sección crítica porque su identificador es mayor al que estaba actualmente. Esto sucede con mayor frecuencia cuando el tiempo de Sleep en el Worker es muy corto.

#### 2. ¿Qué tan eficientemente lo hace y cuál es la desventaja?
Es eficiente porque si da preferencia al Worker que tiene un identificador más alto.
Su desventaja es que como mencionamos anteriormente puede ser que 2 o más Worker pueden entrar a la sección crítica y eso no es lo correcto. Otra desventaja es que si Worker quiere utilizar la sección crítica y su identificador es muy bajo es dificil que este acceda a la misma.



### 3. Tiempo de Lamport

#### 1. ¿Puede darse la situación en que un worker no se le da prioridad al lock a pesar de que envió el request a su lock con un tiempo lógico anterior al worker que lo consiguió?



### Nota
Referencias:
- DISTRIBUTED SYSTEMS Concepts and Design Fifth Edition - 17.5 Distributed deadlocks