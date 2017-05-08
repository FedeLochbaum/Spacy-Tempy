# Para poder corroborar el completo funcionamiento del DNS seguir las siguientes instrucciones en orden.


> - erl -sname rootNamy -setcookie dns
- server:start(server).
- server:start(se, server,se).
- server:start(kth, se,kth).
- host:start(www, www, kth).
- host:start(ftp, ftp, kth).
- resolver:start(server).
- client:test([www, kth, se], resolver).
