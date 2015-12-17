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
#include "parser.h"

struct keyvalue;
typedef struct keyvalue {
    uint8_t *key;
    size_t keylen;
    Buffer *value;
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

// TODO: generalize this into a repo.

// static void
// _loadDirectory(UDPServer *server, char *directory)
// {
//     DIR *fd;
//     if (NULL == (fd = opendir(directory))) {
//         fprintf(stderr, "Error: Failed to open input directory - %s\n", strerror(errno));
//         exit(1);
//     }
//
//     uint8_t buffer[FILE_BUFFER_LENGTH];
//
//     struct dirent* dirEntry;
//     while ((dirEntry = readdir(fd)))
//     {
//         if (!strcmp (dirEntry->d_name, "."))
//             continue;
//         if (!strcmp (dirEntry->d_name, ".."))
//             continue;
//
//         FILE *file = fopen(dirEntry->d_name, "rb");
//         if (file == NULL) {
//             fprintf(stderr, "Error: Failed to open entry file - %s\n", strerror(errno));
//             break;
//         }
//
//         Node *newNode = (Node *) malloc(sizeof(Node));
//         newNode->data = (Buffer *) malloc(sizeof(Buffer));
//         newNode->data->bytes = NULL;
//         newNode->data->length = 0;
//
//         size_t numBytesRead = FILE_BUFFER_LENGTH;
//         while (numBytesRead != 0) {
//             numBytesRead = fread(buffer, 1, FILE_BUFFER_LENGTH, file);
//             if (numBytesRead > 0) {
//                 if (newNode->data->bytes == NULL) {
//                     newNode->data->bytes = (uint8_t *) malloc(numBytesRead);
//                 } else {
//                     newNode->data->bytes = realloc((char **) newNode->data->bytes, newNode->data->length);
//                 }
//                 memcpy((newNode->data->bytes) + newNode->data->length, buffer, numBytesRead);
//                 newNode->data->length += numBytesRead;
//             }
//         }
//
//         fclose(file);
//
//         KeyValue *kv = (KeyValue *) malloc(sizeof(KeyValue));
//         asprintf((char **) &kv->key, "%s", dirEntry->d_name);
//         kv->keylen = strlen(dirEntry->d_name);
//         kv->value = newNode;
//
//         kv->next = server->head;
//         server->head = kv;
//     }
// }

static void
_testParsePacket()
{
    FILE *fp = fopen("data_int", "rb");

    uint8_t header[8];

    if (fp != NULL) {
        // read the header
        fread(header, 1, 8, fp);

        // printf("%d %d\n", header[2], header[3]);

        // get the size
        uint16_t len = ((uint16_t)(header[2]) << 8) | (uint16_t)(header[3]);

        // read the packet into a Buffer
        Buffer *pktBuffer = malloc(sizeof(Buffer));
        pktBuffer->bytes = malloc(len);
        pktBuffer->length = len;
        int numRead = fread(pktBuffer->bytes, 1, len, fp);

        Buffer *name = _readName(pktBuffer->bytes, pktBuffer->length);
        for (size_t i = 0; i < name->length; i++) {
            putc(name->bytes[i], stdout);
        }
    }
}

static Buffer *
_loadContent(UDPServer *server, Buffer *name, Buffer *hash)
{
    KeyValue *curr = server->head;
    while (curr != NULL) {
        if (curr->keylen == name->length) {
            if (memcmp(curr->key, name->bytes, curr->keylen) == 0) {
                return curr->value;
            }
        }
        curr = curr->next;
    }
    return NULL;
}

static void
_displayBuffer(Buffer *Buffer)
{
    for (int i = 0; i < Buffer->length; i++) {
        putc(Buffer->bytes[i], stdout);
    }
}

static void
_loadDataFromFile(UDPServer *server, char *fname)
{
    FILE *fp = fopen(fname, "rb");

    if (fp == NULL) {
        return;
    }

    uint8_t header[8];
    size_t numRead = 1;

    while (numRead > 0) {
        numRead = fread(header, 1, 8, fp);
        if (numRead == 0) {
            break;
        }

        uint16_t len = ((uint16_t)(header[2]) << 8) | (uint16_t)(header[3]);
        Buffer *pktBuffer = malloc(sizeof(Buffer));
        pktBuffer->bytes = malloc(len); // allocate packet header room
        pktBuffer->length = len;
        memcpy(pktBuffer->bytes, header, 8); // move the header into the packet
        int numRead = fread(pktBuffer->bytes + 8, 1, len - 8, fp);

        // Extract the name and hash
        Buffer *name = _readName(pktBuffer->bytes + 8, len - 8);
        Buffer *hash = _readContentObjectHash(pktBuffer->bytes + 8, len - 8);

        KeyValue *kv = (KeyValue *) malloc(sizeof(KeyValue));
        kv->key = malloc(name->length);
        memcpy(kv->key, name->bytes, name->length);
        kv->keylen = name->length;
        kv->value = pktBuffer;

        printf("Repo addition:");
        _displayBuffer(name);
        printf("\n");
        for (int i = 0; i < pktBuffer->length; i++) {
            printf("%02x", pktBuffer->bytes[i]);
        }
        printf("\n");

        kv->next = server->head;
        server->head = kv;
    }
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
    _loadDataFromFile(&server, argv[2]);

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
    uint8_t buffer[MTU];
    for (;;) {
        bzero(buffer, MTU);
        numBytesReceived = recvfrom(server.socket, buffer, MTU, 0, (struct sockaddr *) &clientAddress, &clientlen);
        if (numBytesReceived < 0) {
            LogFatal("recvfrom() failed");
        } else {
            printf("Received %d bytes\n", numBytesReceived);
        }

        // 1. get the name of the packet
        uint16_t len = ((uint16_t)(buffer[2]) << 8) | (uint16_t)(buffer[3]);
        Buffer *name = _readName(buffer + 8, len - 8);
        Buffer *hash = _readContentObjectHash(buffer + 8, len - 8);

        // 2) index into the repo to get the packet
        Buffer *content = _loadContent(&server, name, hash);
        if (content != NULL) {
            printf("Sending [%zu]:\n", content->length);
            // for (int i = 0; i < content->length; i++) {
            //     printf("%02x", content->bytes[i]);
            // }
            // printf("\n");
            if (sendto(server.socket, content->bytes, content->length, 0,
            	(struct sockaddr *) &clientAddress, clientlen) < 0) {
                LogFatal("Error sending content object response\n");
            }
        } else {
            printf("NOT FOUND!?!?!?!\n");
        }
    }

    close(server.socket);

    return 0;
}
