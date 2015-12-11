#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "util.h"

int
main(int argc, char** argv)
{
    int socketfd;
    struct sockaddr_in servaddr,cliaddr;
    char serverResponseBuffer[MTU];
    int bytesReceived;
    int totalBytesRcvd;

    if (argc != 4) {
        fprintf(stderr, "usage: %s <Server IP Address> <Port> <Packet File Name>\n", argv[0]);
        exit(1);
    }

    char *serverIPAddress = argv[1];
    int serverPort = atoi(argv[2]);
    char *fileName = argv[3];

    FILE *fp = fopen(fileName, "rb");
    if (fp == NULL) {
        fprintf(stderr, "Error: could not open the file in 'rb' mode\n");
        exit(1);
    }

    printf("Creating the list\n");

    Node *tail = (Node *) malloc(sizeof(Node));
    tail->next = NULL;
    tail->data = (blob *) malloc(sizeof(blob));
    ssize_t read;
    int lineno = 0;
    while ((tail->data->bytes = (uint8_t *) fgetln(fp, &tail->data->length)) != NULL) {
        printf("Read number %d\n", lineno);
        lineno++;

        Node *curr = (Node *) malloc(sizeof(Node));
        curr->next = tail;
        tail = curr;
    }

    Node *head = tail;

    socketfd = socket(AF_INET, SOCK_DGRAM, 0);
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = inet_addr(serverIPAddress);
    servaddr.sin_port = htons(serverPort);

    printf("Starting the piper\n");

    TimeBlock(stdout, {

        while (head != NULL) {
            if (sendto(socketfd, head->data->bytes, head->data->length, 0,
                (struct sockaddr *) &servaddr, sizeof(servaddr)) != head->data->length) {
                LogFatal("send() failed");
            }
            head = head->next;

#if DEBUG
            fprintf(stderr, "Received: \n");
#endif

            // bytesReceived = recv(socketfd, serverResponseBuffer, RCVBUFSIZE, 0);
            // totalBytesRcvd += bytesReceived;
            //
            // // printf("%.*s", bytesReceived, serverResponseBuffer);
            //
            // if (bytesReceived < RCVBUFSIZE) {
            //     break;
            // }
        }

#if DEBUG
        fprintf(stderr, "\n");
#endif

    });

    close(socketfd);

    return 0;
}
