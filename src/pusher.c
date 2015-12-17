#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <getopt.h>

#include "util.h"
#include "parser.h"

typedef enum {
    PusherMode_Ping,
    PusherMode_Flood
} PusherMode;

typedef struct PusherStatEntry {
    uint64_t sentTime;
    uint64_t receivedTime;
    uint64_t rtt;
    size_t size;
    size_t seqNumber;
} PusherStatEntry;

typedef struct PacketTable {
    size_t numberOfPackets;
    Buffer **packets;
    PusherStatEntry **stats;
} PacketTable;

typedef struct PusherOptions {
    bool showUsageStats;
    size_t windowSize;
    PusherMode mode;
    char *fwdIPAddress;
    int fwdPort;
    char *packetFileName;
} PusherOptions;

typedef struct PusherQueueEntry {
    int number;
    Buffer *name;
} PusherQueueEntry;

typedef struct Pusher {
    PacketTable *table;

    size_t windowSize;
    size_t outstanding;

    int socketfd;
    struct sockaddr_in fwdaddr;
} Pusher;

void
showUsage()
{
    printf("Usage: pusher <options> <server IP address> <port> <packet file>\n");
    printf(" -p       --ping              Stop-and-wait mode\n");
    printf(" -f       --flood             Flood mode\n");
    printf(" -r       --rusage            Print rusage\n");
    printf(" -w num   --window num        Stop and wait window size\n");
    printf(" -h       --help              Display the help message\n");
}

PusherOptions *
parseCommandLineOptions(int argc, char **argv)
{
    static struct option longopts[] = {
            { "ping",       no_argument,        NULL,'p' },
            { "flood",      no_argument,        NULL,'f' },
            { "outstanding",required_argument,  NULL,'o' },
            { "rusage",     no_argument,        NULL,'r'},
            { "help",       no_argument,        NULL,'h'},
            { NULL,0,NULL,0}
    };

    if (argc < 4) {
        showUsage();
        exit(EXIT_FAILURE);
    }

    PusherOptions *options = malloc(sizeof(PusherOptions));
    options->windowSize = 10;

    int c;
    while (optind < argc) {
        if ((c = getopt_long(argc, argv, "rphfo:", longopts, NULL)) != -1) {
            switch(c) {
                case 'r':
                    options->showUsageStats = true;
                    break;
                case 'p':
                    options->mode = PusherMode_Ping;
                    break;
                case 'f':
                    options->mode = PusherMode_Flood;
                    break;
                case 'w':
                    sscanf(optarg, "%zu", &(options->windowSize));
                    break;
                case 'h':
                    showUsage();
                    exit(EXIT_SUCCESS);
                default:
                    break;
            }
        } else { // handle the rest of the mandatory options
            asprintf(&(options->fwdIPAddress), "%s", argv[optind++]);
            options->fwdPort = atoi(argv[optind++]);
            asprintf(&(options->packetFileName), "%s", argv[optind++]);
        }
    }

    return options;
};

Buffer *
loadPacketFromFile(FILE *fp)
{
    uint8_t header[8];
    int numRead = fread(header, 1, 8, fp);

    if (numRead == 0) {
        return NULL;
    } else {
        uint16_t len = ((uint16_t)(header[2]) << 8) | (uint16_t)(header[3]);
        Buffer *data = (Buffer *) malloc(sizeof(Buffer));
        data->bytes = malloc(len); // allocate packet header room
        data->length = len;
        memcpy(data->bytes, header, 8); // move the header into the packet
        int numRead = fread(data->bytes + 8, 1, len - 8, fp);
        if (numRead != (len - 8)) { // error: malformed packet stream
            fprintf(stderr, "Error: read the incorrect number of bytes, got %d expected %d\n", numRead, len - 8);
            exit(EXIT_FAILURE);
        }

        return data;
    }
}

PacketTable *
buildPacketTableFromFile(char *fileName)
{
    FILE *fp = fopen(fileName, "rb");
    if (fp == NULL) {
        fprintf(stderr, "Error: could not open the file in 'rb' mode\n");
        exit(1);
    }

    printf("Creating the table\n");

    PacketTable *table = malloc(sizeof(PacketTable));

    Node *tail = (Node *) malloc(sizeof(Node));
    tail->next = NULL;
    tail->data = (Buffer *) malloc(sizeof(Buffer));

    uint8_t header[8];
    size_t numRead = 1;

    int numberOfPackets = 0;
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

        // Create the node and add it to the list
        Node *curr = (Node *) malloc(sizeof(Node));
        curr->data = (Buffer *) malloc(sizeof(Buffer));
        curr->next = tail;
        tail = curr;
        numberOfPackets++;

        printf("Added entry (interest) of length %d\n", numRead + 8);
    }
    Node *head = tail->next;

    // Populate the table and packet stats
    table->numberOfPackets = numberOfPackets;
    table->packets = (Buffer **) malloc(sizeof(Buffer *) * numberOfPackets);
    table->stats = (PusherStatEntry **) malloc(sizeof(PusherStatEntry *) * numberOfPackets);

    Node *curr = head;
    for (int i = 0; i < numberOfPackets; i++) {
        table->packets[i] = curr->data;
        table->stats[i] = (PusherStatEntry *) malloc(sizeof(PusherStatEntry));
        bzero(table->stats[i], sizeof(PusherStatEntry));
        curr = curr->next;
    }

    return table;
}

Pusher *
initializePusher(PusherOptions *options)
{
    Pusher *pusher = (Pusher *) malloc(sizeof(Pusher));

    pusher->table = buildPacketTableFromFile(options->packetFileName);
    pusher->socketfd = socket(AF_INET, SOCK_DGRAM, 0);
    bzero(&pusher->fwdaddr, sizeof(pusher->fwdaddr));
    pusher->fwdaddr.sin_family = AF_INET;
    pusher->fwdaddr.sin_addr.s_addr = inet_addr(options->fwdIPAddress);
    pusher->fwdaddr.sin_port = htons(options->fwdPort);

    pusher->outstanding = 0;
    pusher->windowSize = options->windowSize;

    return pusher;
}

uint64_t
getCurrentTimeUs()
{
    struct timeval now;
    gettimeofday(&now, NULL);
    return ((now.tv_sec * 1000000L) + now.tv_usec);
}

Pusher *
runPusher(Pusher *pusher)
{
    int bytesReceived = 0;
    int totalBytesRcvd = 0;
    int packetNumber = 0;
    uint8_t serverResponseBuffer[MTU];

    TimeBlockUs(stdout, {
        while (packetNumber < pusher->table->numberOfPackets) {
            Buffer *packet = pusher->table->packets[packetNumber];
            if (sendto(pusher->socketfd, packet->bytes, packet->length, 0,
                (struct sockaddr *) &pusher->fwdaddr, sizeof(pusher->fwdaddr)) != packet->length) {
                LogFatal("send() failed");
            }
            packetNumber++;
        }
        packetNumber = 0;
        while (packetNumber < pusher->table->numberOfPackets) {
            bytesReceived = recv(pusher->socketfd, serverResponseBuffer, MTU, 0);
            totalBytesRcvd += bytesReceived;
            packetNumber++;
        }
    });

    return pusher;
}

Pusher *
runPusherPerPacket(Pusher *pusher)
{
    int bytesReceived = 0;
    int totalBytesRcvd = 0;
    uint8_t serverResponseBuffer[MTU];

    PusherQueueEntry **queue = (PusherQueueEntry **) malloc(sizeof(PusherQueueEntry *) * pusher->windowSize);
    for (int i = 0; i < pusher->windowSize; i++) {
        queue[i] = (PusherQueueEntry *) malloc(sizeof(PusherQueueEntry));
        queue[i]->number = 0;
        queue[i]->name = NULL;
    }
    int queueStart = 0;
    int queueEnd = pusher->windowSize - 1;
    int queueSize = pusher->windowSize;

    int packetNumber = 0;
    Buffer *packet = pusher->table->packets[packetNumber];
    PusherStatEntry *stats = pusher->table->stats[packetNumber];

    TimeBlockUs(stdout, {
        while (packetNumber < pusher->table->numberOfPackets) {

            // Try to send (fill the pipe)
            // while (pusher->outstanding < pusher->windowSize) {

            packet = pusher->table->packets[packetNumber];
            stats = pusher->table->stats[packetNumber];

            // TODO: FIX THE QUEUE MGMT AND MATCHING LOGIC

            while (pusher->outstanding < pusher->windowSize) {
                printf("Sending %zu bytes\n", packet->length);

                if (sendto(pusher->socketfd, packet->bytes, packet->length, 0,
                    (struct sockaddr *) &pusher->fwdaddr, sizeof(pusher->fwdaddr)) != packet->length) {
                    LogFatal("send() failed");
                }

                // Log the start time
                stats->sentTime = getCurrentTimeUs();
                stats->size = packet->length;
                stats->seqNumber = packetNumber;
                stats->rtt = 0;

                // Read the name (identifier) and record the packet that was sent in this slot
                Buffer *name = _readName(packet->bytes + 8, packet->length - 8);
                queue[queueStart]->number = packetNumber;
                queue[queueStart]->name = name;

                pusher->outstanding++;
                queueStart = (queueStart + 1) % queueSize;

                packetNumber++;
            }

            // Try to receive to clear the pipe
            if (pusher->outstanding > 0) {
                bytesReceived = recv(pusher->socketfd, serverResponseBuffer, MTU, 0);
                if (bytesReceived > 0) {
                    totalBytesRcvd += bytesReceived;
                    fprintf(stderr, "Received [%d]: \n", bytesReceived);
                    // for (int i = 0; i < bytesReceived; i++) {
                    //     printf("%02x", serverResponseBuffer[i]);
                    // }
                    // printf("\n");

                    Buffer *name = _readName(serverResponseBuffer + 8, bytesReceived - 8);
                    int requestNumber = 0;
                    for (int i = 0; i < queueSize; i++) {
                        if (queue[i]->name->length == name->length) {
                            if (memcmp(queue[i]->name->bytes, name->bytes, name->length) == 0) {
                                requestNumber = queue[i]->number;
                                break;
                            }
                        }
                    }

                    // Update the per-request stats
                    stats = pusher->table->stats[requestNumber];
                    stats->receivedTime = getCurrentTimeUs();
                    stats->rtt = stats->receivedTime - stats->sentTime;

                    printf("Packet %d RTT %zu\n", requestNumber, stats->rtt);

                    pusher->outstanding--;
                    // queueEnd = (queueEnd + 1) % queueSize;
                }
            }
        }
    });

    printf("Total packets: %zu\n", pusher->table->numberOfPackets);

    close(pusher->socketfd);

    return pusher;
}

void
processPusherStats(Pusher *pusher)
{
    uint64_t sum = 0;

    for (int i = 0; i < pusher->table->numberOfPackets; i++) {
        sum += pusher->table->stats[i]->rtt;
    }
    uint64_t averageRtt = (sum / pusher->table->numberOfPackets);
    printf("Average RTT = %zu\n", averageRtt);
}

int
main(int argc, char** argv)
{
    PusherOptions *options = parseCommandLineOptions(argc, argv);
    if (options == NULL) {
        return EXIT_FAILURE;
    }

    Pusher *pusher = initializePusher(options);
    // runPusher(pusher);
    runPusherPerPacket(pusher);
    processPusherStats(pusher);

    // int socketfd;
    // struct sockaddr_in servaddr,cliaddr;
    // char serverResponseBuffer[MTU];
    // int bytesReceived;
    // int totalBytesRcvd;
    //
    // if (argc != 4) {
    //     fprintf(stderr, "usage: %s <Server IP Address> <Port> <Packet File Name>\n", argv[0]);
    //     exit(1);
    // }
    //
    // char *serverIPAddress = argv[1];
    // int serverPort = atoi(argv[2]);
    // char *fileName = argv[3];
    //
    // FILE *fp = fopen(fileName, "rb");
    // if (fp == NULL) {
    //     fprintf(stderr, "Error: could not open the file in 'rb' mode\n");
    //     exit(1);
    // }
    //
    // printf("Creating the list\n");
    //
    // Node *tail = (Node *) malloc(sizeof(Node));
    // tail->next = NULL;
    // tail->data = (Buffer *) malloc(sizeof(Buffer));
    //
    // uint8_t header[8];
    // size_t numRead = 1;
    //
    // int numberOfPackets = 0;
    // while (numRead > 0) {
    //     bzero(header, 8);
    //     numRead = fread(header, 1, 8, fp);
    //     if (numRead == 0) {
    //         break;
    //     }
    //
    //     uint16_t len = ((uint16_t)(header[2]) << 8) | (uint16_t)(header[3]);
    //     tail->data->bytes = malloc(len); // allocate packet header room
    //     tail->data->length = len;
    //     memcpy(tail->data->bytes, header, 8); // move the header into the packet
    //     int numRead = fread(tail->data->bytes + 8, 1, len - 8, fp);
    //     if (numRead != (len - 8)) {
    //         fprintf(stderr, "Error: read the incorrect number of bytes, got %d expected %d\n", numRead, len - 8);
    //         exit(1);
    //     }
    //
    //     Node *curr = (Node *) malloc(sizeof(Node));
    //     curr->data = (Buffer *) malloc(sizeof(Buffer));
    //     curr->next = tail;
    //     tail = curr;
    //     numberOfPackets++;
    //
    //     printf("Added entry (interest) of length %d\n", numRead + 8);
    // }
    //
    // Node *head = tail->next;
    //
    // socketfd = socket(AF_INET, SOCK_DGRAM, 0);
    // bzero(&servaddr, sizeof(servaddr));
    // servaddr.sin_family = AF_INET;
    // servaddr.sin_addr.s_addr = inet_addr(serverIPAddress);
    // servaddr.sin_port = htons(serverPort);
    //
    // printf("Starting the packet pusher\n");
    //
    // int numPackets = 0;
    // TimeBlockUs(stdout, {
    //
    //     while (head != NULL) {
    //         printf("Sending %d bytes\n", head->data->length);
    //         if (sendto(socketfd, head->data->bytes, head->data->length, 0,
    //             (struct sockaddr *) &servaddr, sizeof(servaddr)) != head->data->length) {
    //             LogFatal("send() failed");
    //         }
    //
    //         head = head->next;
    //     }
    //
    //     while (numPackets < numberOfPackets) {
    //         bytesReceived = recv(socketfd, serverResponseBuffer, MTU, 0);
    //         totalBytesRcvd += bytesReceived;
    //         fprintf(stderr, "Received [%d]: \n", bytesReceived);
    //         for (int i = 0; i < bytesReceived; i++) {
    //             printf("%02x", serverResponseBuffer[i]);
    //         }
    //         printf("\n");
    //
    //         numPackets--;
    //     }
    // });
    //
    // printf("Total packets: %d\n", numPackets);
    // close(socketfd);
    //
    // return EXIT_SUCCESS;
}
