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

    uint8_t header[8];
    size_t numRead = 1;

    while (numRead > 0) {
        bzero(header, 8);
        numRead = fread(header, 1, 8, fp);
        if (numRead == 0) {
            break;
        }

        uint16_t len = ((uint16_t)(header[2]) << 8) | (uint16_t)(header[3]);
        tail->data->bytes = malloc(len); // allocate packet header room
        tail->data->length = len;
        memcpy(tail->data->bytes, header, 8); // move the header into the packet
        int numRead = fread(tail->data->bytes + 8, 1, len - 8, fp);
        if (numRead != (len - 8)) {
            fprintf(stderr, "Error: read the incorrect number of bytes, got %d expected %d\n", numRead, len - 8);
            exit(1);
        }

        Node *curr = (Node *) malloc(sizeof(Node));
        curr->data = (blob *) malloc(sizeof(blob));
        curr->next = tail;
        tail = curr;

        printf("Added entry of length %d\n", numRead + 8);
    }

    Node *head = tail->next;

    socketfd = socket(AF_INET, SOCK_DGRAM, 0);
    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = inet_addr(serverIPAddress);
    servaddr.sin_port = htons(serverPort);

    printf("Starting the piper\n");

    int numPackets = 0;
    TimeBlock(stdout, {

        while (head != NULL) {
            printf("Sending %d bytes\n", head->data->length);
            if (sendto(socketfd, head->data->bytes, head->data->length, 0,
                (struct sockaddr *) &servaddr, sizeof(servaddr)) != head->data->length) {
                LogFatal("send() failed");
            }

            head = head->next;
            numPackets++;

            bytesReceived = recv(socketfd, serverResponseBuffer, MTU, 0);
            totalBytesRcvd += bytesReceived;
            fprintf(stderr, "Received [%d]: \n", bytesReceived);
            for (int i = 0; i < bytesReceived; i++) {
                printf("%02x", serverResponseBuffer[i]);
            }
            printf("\n");
        }
    });

    printf("Total packets: %d\n", numPackets);
    close(socketfd);

    return 0;
}
