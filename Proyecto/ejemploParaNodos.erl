Node1: erl -sname ser1 -setcookie prueba

Node2: erl -sname ser2 -setcookie prueba

Node3: erl -sname ser3 -setcookie prueba
---------------------------------------

Node1: distributedServer:start(s1, {0,0}, {50,50}, {100,100}, 1000).

Node2: distributedServer:start(s2, {0,50}, {50,100}, {100,100}, 1000).

Node2: distributedServer:start(s3, {50,50}, {100,100}, {100,100}, 1000).

Node3: distributedServer:start(s4, {50,0}, {100,50}, {100,100}, 1000).
---------------------------------------------------------------------


Node1: s1 ! {peers, [{s2, ser2@QueBuenServer}, {s3, ser2@QueBuenServer}, {s4, ser3@QueBuenServer}], {s2, ser2@QueBuenServer}}.

Node2: s2 ! {peers, [{s3, ser2@QueBuenServer}, {s4, ser3@QueBuenServer}, {s1, ser1@QueBuenServer}], {s3, ser2@QueBuenServer}}.

Node2: s3 ! {peers, [{s4, ser3@QueBuenServer}, {s1, ser1@QueBuenServer}, {s2, ser2@QueBuenServer}], {s4, ser3@QueBuenServer}}.

Node3: s4 ! {peers, [{s1, ser1@QueBuenServer}, {s2, ser2@QueBuenServer}, {s3, ser2@QueBuenServer}], {s1, ser1@QueBuenServer}}.
-----------------------------------------------------------------------------------------------------------------------------

Node1: manager:start(manager1, 2000).
Node1: manager1 ! {servers, [s1]}.

Node2: manager:start(manager2, 2000).
Node2: manager2 ! {servers, [s2,s3]}.

Node3: manager:start(manager3, 2000).
Node3: manager3 ! {servers, [s4]}.
----------------------------------

Node1: manager1 ! {managers, [{manager2, ser2@QueBuenServer}, {manager3, ser3@QueBuenServer}], {monitors, {manager2, ser2@QueBuenServer}, {manager3, ser3@QueBuenServer}}}.

Node2: manager2 ! {managers, [{manager1, ser1@QueBuenServer}, {manager3, ser3@QueBuenServer}], {monitors, {manager3, ser3@QueBuenServer}, {manager1, ser1@QueBuenServer}}}.

Node3: manager3 ! {managers, [{manager1, ser1@QueBuenServer}, {manager2, ser2@QueBuenServer}], {monitors, {manager1, ser1@QueBuenServer}, {manager2, ser2@QueBuenServer}}}.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------



Luego para probar la adicion de nuevos managers:

Node4: erl -sname ser4 -setcookie prueba
Node4: manager:newManager({manager4, ser4@QueBuenServer}, [{manager1, ser1@QueBuenServer}, {manager2, ser2@QueBuenServer}, {manager3, ser3@QueBuenServer}], 2000).
