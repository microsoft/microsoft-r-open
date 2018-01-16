cc -c -o Rserv.o Rserv.c -DDAEMON -I/usr/share/R/include -Iinclude
cc    -c -o session.o session.c -DDAEMON
cc    -c -o md5.o md5.c -DDAEMON
cc -o Rserve Rserv.o session.o md5.o -L/usr/lib64/R/lib -lR -ldl -lcrypt

cc -c -o Rserv_d.o Rserv.c -DNODAEMON -DRSERV_DEBUG -g -DRSERV_DEBUG -g -I/usr/share/R/include -Iinclude
cc -o Rserve.dbg Rserv_d.o session.o md5.o -L/usr/lib64/R/lib -lR -ldl -lcrypt

