#ifndef TCP_H

#define TCP_H

#include "common.h"
#include "obj.h"

struct CallbackArgs {
    VM *vm;
    int sockfd;
    Chan *chan;
};

typedef struct CallbackArgs CallbackArgs;

struct TcpServer {
    VM *vm;
    int port;
    Chan *chan;
};

typedef struct TcpServer TcpServer;

void *tcp_server_thread(void *arg);
void start_tcp_server(VM *vm, int port, Chan *chan);
void handle_tcp_socket(VM *vm, int sockfd, Chan *chan);
void *thread_callback(void *arg);

#endif
