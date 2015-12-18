#include <stdio.h>
#include <stdlib.h>

#ifndef UTIL_H_
#define UTIL_H_

#include <sys/time.h>
#include <time.h>

#define MTU (64*1024)

#define FILE_BUFFER_LENGTH 256

#define MAX_NUMBER_OF_TCP_CONNECTIONS 5
#define MAX_NUMBER_OF_SCTP_OSTREAMS 5
#define MAX_NUMBER_OF_SCTP_ISTREAMS 4
#define MAX_NUMBER_OF_SCTP_ATTEMPTS 4

#define DEBUG 0

typedef unsigned char bool;
#define true 1
#define false 0

typedef struct timeval TimeValue;

void LogFatal(char *errorMessage);

#define TimeBlockUs(_out, _block) \
    struct timeval start; \
    struct timeval end; \
    struct timeval delta; \
    gettimeofday(&start, NULL); \
    _block \
    gettimeofday(&end, NULL); \
    timersub(&end, &start, &delta); \
    fprintf(_out, "%lu\n", (delta.tv_sec * 1000000L) + delta.tv_usec);

// Dynamically sized buffer
typedef struct {
    uint8_t *bytes;
    size_t length;
} Buffer;

// TODO: use overlays instead of memcpy everywhere
typedef struct {
    size_t offset;
    size_t length;
} BufferOverlay;

// general linked list
struct node;
typedef struct node {
    Buffer *data;
    struct node *next;
} Node;

#endif // UTIL_H_
