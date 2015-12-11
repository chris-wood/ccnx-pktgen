#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <dirent.h>
#include <errno.h>

#include "util.h"

struct keyvalue;
typedef struct keyvalue {
    uint8_t *key;
    Node *value;
    struct keyvalue *next;
} KeyValue;

typedef struct {
    int port;
    int socket;
    struct sockaddr_in serverAddress;
    char *hostAddress;

    // table of contents in memory
    KeyValue *head;
} UDPServer;

static void
_loadDirectory(UDPServer *server, char *directory)
{
    DIR *fd;
    if (NULL == (fd = opendir(directory))) {
        fprintf(stderr, "Error: Failed to open input directory - %s\n", strerror(errno));
        exit(1);
    }

    uint8_t buffer[FILE_BUFFER_LENGTH];

    struct dirent* dirEntry;
    while ((dirEntry = readdir(fd)))
    {
        if (!strcmp (dirEntry->d_name, "."))
            continue;
        if (!strcmp (dirEntry->d_name, ".."))
            continue;

        FILE *file = fopen(dirEntry->d_name, "rb");
        if (file == NULL) {
            fprintf(stderr, "Error: Failed to open entry file - %s\n", strerror(errno));
            break;
        }

        Node *newNode = (Node *) malloc(sizeof(Node));
        newNode->data = (blob *) malloc(sizeof(blob));
        newNode->data->bytes = NULL;
        newNode->data->length = 0;

        size_t numBytesRead = FILE_BUFFER_LENGTH;
        while (numBytesRead != 0) {
            numBytesRead = fread(buffer, 1, FILE_BUFFER_LENGTH, file);
            if (numBytesRead > 0) {
                if (newNode->data->bytes == NULL) {
                    newNode->data->bytes = (uint8_t *) malloc(numBytesRead);
                } else {
                    newNode->data->bytes = realloc((char **) newNode->data->bytes, newNode->data->length);
                }
                memcpy((newNode->data->bytes) + newNode->data->length, buffer, numBytesRead);
                newNode->data->length += numBytesRead;
            }
        }

        fclose(file);

        KeyValue *kv = (KeyValue *) malloc(sizeof(KeyValue));
        asprintf((char **) &kv->key, "%s", dirEntry->d_name);
        kv->value = newNode;

        kv->next = server->head;
        server->head = kv;
    }
}

static blob *
_readName(blob *packetData)
{
    return NULL;
}

static blob *
_readContentObjectHash(blob *packetData)
{
    return NULL;
}

int
main(int argc, char **argv)
{
    socklen_t clientlen;
    struct sockaddr_in clientAddress;
    struct hostent *hostp;
    int numBytesReceived;
    UDPServer server;

    if (argc != 3) {
        fprintf(stderr, "Usage: %s <Server Port> <Directory>\n", argv[0]);
        exit(1);
    }

    // Setup the server port
    server.port = atoi(argv[1]);

    // Load the contents of the directory into memory
    _loadDirectory(&server, argv[2]);

    if ((server.socket = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        LogFatal("socket() failed");
    }

    // set address reuse
    int optval = 1;
    setsockopt(server.socket, SOL_SOCKET, SO_REUSEADDR, (const void *) &optval , sizeof(int));

    bzero((char *) &server.serverAddress, sizeof(server.serverAddress));
    server.serverAddress.sin_family = AF_INET;
    server.serverAddress.sin_addr.s_addr = htonl(INADDR_ANY);
    server.serverAddress.sin_port = htons(server.port);

    if (bind(server.socket, (struct sockaddr *) &(server.serverAddress), sizeof(server.serverAddress)) < 0) {
        LogFatal("bind() failed");
    }

    clientlen = sizeof(clientAddress);
    char buffer[MTU];
    bzero(buffer, MTU);

    for (;;) {
        numBytesReceived = recvfrom(server.socket, buffer, MTU, 0, (struct sockaddr *) &clientAddress, &clientlen);
        if (numBytesReceived < 0) {
            LogFatal("recvfrom() failed");
        }

        // TODO;
        // 1) decode the packet to obtain the name and COH
        // 2) index into the repo to get the packet
        // 3) reply with the appropriate response
    }

    close(server.socket);

    return 0;
}
