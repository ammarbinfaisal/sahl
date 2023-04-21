#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h> // read(), write(), close()

#include "common.h"
#include "conc.h"
#include "list.h"
#include "tcp.h"
#include "gc.h"

void *thread_callback(void *arg) {
    CallbackArgs *args = (CallbackArgs *)arg;
    handle_tcp_socket(args->vm, args->sockfd, args->chan);
    return NULL;
}

void *tcp_server_thread(void *arg) {
    TcpServer *args = (TcpServer *)arg;
    start_tcp_server(args->vm, args->port, args->chan);
    return NULL;
}

void start_tcp_server(VM *vm, int port, Chan *chan) {
    int sockfd, newsockfd, clilen;
    char buffer[256];
    struct sockaddr_in serv_addr, cli_addr;
    int n;

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        error(vm, "ERROR opening socket");
    }
    bzero((char *)&serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(port);
    if (bind(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        error(vm, "ERROR on binding");
    }
    listen(sockfd, 5);

    LinkedList *list_threads = new_list();
    LinkedList *tail_threads = list_threads;

    LinkedList *list_args = new_list();
    LinkedList *tail_args = list_args;

    while (1) {
        if (chan->closed) {
            LinkedList *tmp;
            tmp = list_threads;
            while (tmp != NULL) {
                pthread_join(*(pthread_t *)tmp->data, NULL);
                tmp = tmp->next;
            }
            list_free(list_threads);
            tmp = list_args;
            while (tmp != NULL) {
                free(tmp->data);
                tmp = tmp->next;
            }
            close(sockfd);
            return;
        }
        clilen = sizeof(cli_addr);
        newsockfd = accept(sockfd, (struct sockaddr *)&cli_addr, &clilen);
        if (newsockfd < 0) {
            error(vm, "error while accepting a connection in tcp server");
        }
        pthread_t thread;
        CallbackArgs *args = malloc(sizeof(CallbackArgs));
        args->vm = vm;
        args->chan = chan;
        args->sockfd = newsockfd;
        list_append(&tail_args, args);
        list_append(&tail_threads, &thread);
        pthread_create(&thread, NULL, thread_callback, (void *)args);
        APPEND_THREAD(thread);
    }
}

void handle_tcp_socket(VM *vm, int sockfd, Chan *chan) {
    char buffer[256];
    int n;
    Chan *respchan = new_chan(128);
    Obj *obj_respchan = new_obj(vm, OBJ_CHAN);
    obj_respchan->channel.chan = respchan;
    Chan *reqchan = new_chan(128);
    Obj *obj_reqchan = new_obj(vm, OBJ_CHAN);
    obj_reqchan->channel.chan = reqchan;
    Obj *chan_list = new_obj(vm, OBJ_LIST);
    chan_list->list.items = allocate(vm, sizeof(Value) * 2);
    chan_list->list.items[0] = OBJ_VAL(obj_reqchan);
    chan_list->list.items[1] = OBJ_VAL(obj_respchan);
    chan_list->list.length = 2;
    chan_list->list.capacity = 2;
    chan_write(chan, OBJ_VAL(chan_list));
    while (1) {
        if (respchan->closed) {
            close(sockfd);
            return;
        }
        bzero(buffer, 256);
        n = read(sockfd, buffer, 255);
        if (n < 0) {
            error(vm, "ERROR reading from socket");
        }
        if (n == 0) {
            close_chan(respchan);
            close(sockfd);
            return;
        }
        Obj *obj = new_obj(vm, OBJ_STRING);
        obj->string.data = malloc(n);
        memcpy(obj->string.data, buffer, n);
        chan_write(reqchan, OBJ_VAL(obj));
        Value v;
        chan_read(respchan, &v);
        char *str = stringify(v);
        n = write(sockfd, str, strlen(str));
        if (n < 0) {
            error(vm, "ERROR writing to socket");
        }
    }
}
