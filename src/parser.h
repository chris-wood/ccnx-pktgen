#ifndef PARSER_H_
#define PARSER_H_

#include "util.h"

size_t
_getNameLength(uint8_t *buffer, size_t length);

size_t
_getNameIndex(uint8_t *buffer, size_t length);

size_t
_getContentHashIndex(uint8_t *buffer, size_t length);

size_t
_getContentHashLength(uint8_t *buffer, size_t length);

Buffer *
_readName(uint8_t *buffer, size_t length);

Buffer *
_readContentObjectHash(uint8_t *buffer, size_t length);

#endif // PARSER_H_
