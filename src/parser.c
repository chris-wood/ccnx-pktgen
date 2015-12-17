#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"

size_t
_getNameLength(uint8_t *buffer, size_t length)
{
    int offset = 6; // 4 for TL, 2 for T of the name
    uint16_t len = ((uint16_t)(buffer[offset]) << 8) | (uint16_t)(buffer[offset + 1]);
    return (size_t) len;
}

size_t
_getNameIndex(uint8_t *buffer, size_t length)
{
    return 8; // 8 + 4 + 4
}

size_t
_getContentHashIndex(uint8_t *buffer, size_t length) // skip past the name
{
    return _getNameIndex(buffer, length) + _getNameLength(buffer, length) + 4;
}

size_t
_getContentHashLength(uint8_t *buffer, size_t length) // skip past the name
{
    int offset = _getNameIndex(buffer, length) + _getNameLength(buffer, length) + 2;
    uint16_t len = ((uint16_t)(buffer[offset]) << 8) | (uint16_t)(buffer[offset + 1]);
    return (size_t) len;
}

Buffer *
_readName(uint8_t *buffer, size_t length)
{
    size_t len = _getNameLength(buffer, length);
    Buffer *b = (Buffer *) malloc(sizeof(Buffer));
    b->bytes = malloc(len);
    b->length = len;
    memcpy(b->bytes, buffer + _getNameIndex(buffer, length), len);
    return b;
}

Buffer *
_readContentObjectHash(uint8_t *buffer, size_t length)
{
    size_t len = _getContentHashLength(buffer, length);
    Buffer *b = (Buffer *) malloc(sizeof(Buffer));
    b->bytes = malloc(len);
    b->length = len;
    memcpy(b->bytes, buffer + _getContentHashIndex(buffer, length), len);
    return b;
}
