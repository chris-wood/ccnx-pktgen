# Overview

[![Build Status](https://travis-ci.org/chris-wood/ccnx-pktgen.svg?branch=master)](https://travis-ci.org/chris-wood/ccnx-pktgen)

ccnx-pktgen is a CCNx packet grammar parser and generator that converts 
grammar specifications to raw bytes. It can also be used to quickly create
packets represented in S-expression form. The tool is meant to be a general
purpose packet generator for tests and experiments. It is also intended to 
guide packet format experimentation. 

# Requirements 

- The Glorious Glasgow Haskell Compilation System, version 7.10.2 (I have not tested on past versions.)

# Grammar Specification Notes

The current CCNx packet grammar is specified online at:

https://tools.ietf.org/html/draft-irtf-icnrg-ccnxmessages-00#section-3.6.1

# CCNx Packets in S-Expression Form

NameSegment := NameSegment <String>
Name := Name NameSegment-1 NameSegment-2 ...
Message := Message Name Payload
Validation := Validation ValidationAlg ValidationPayload
Interest := Interest Name | InterestWithPayload Message | SignedInterest Message Validation
Content := NamelessContent Payload | Content Message | SignedContent Message Validation
FixedHeader := FixedHeader Version PacketType PacketLength 

Packet := ByteString

# Output Formats

ccnx-pktgen generates packet pairs of wire-format-encoded packets. 
These raw buffers can be written to files or used in other ways.

# Static Examples

Most generation functions accept or require a parameter of
type RandomGen. This is used to generate random data for the 
correpsonding packet element. For example, the RandomGen is used
to produce name components in the *gen_name* function (see below).

## Names

```
*Parser> gen_name 3 (mkStdGen 42)
Name {components = [NameComponent {value = "poyvibqxfn"},NameComponent {value = "oyvibq"},NameComponent {value = "yvi"}]}

*Parser> toTLV (gen_name 3 (mkStdGen 42))
NestedTLV {tlv_type = TType 0 0, tlv_length = Length 0 31, tlv_nested_value = [RawTLV {tlv_type = TType 0 1, tlv_length = Length 0 10, tlv_raw_value = "poyvibqxfn"},RawTLV {tlv_type = TType 0 1, tlv_length = Length 0 6, tlv_raw_value = "oyvibq"},RawTLV {tlv_type = TType 0 1, tlv_length = Length 0 3, tlv_raw_value = "yvi"}]}

*Parser> serialize (toTLV (gen_name 3 (mkStdGen 42)))
"\NUL\NUL\NUL\US\NUL\SOH\NUL\npoyvibqxfn\NUL\SOH\NUL\ACKoyvibq\NUL\SOH\NUL\ETXyvi"
```

## Payloads

```
*Parser> gen_payload 10
Payload {bytes = [37,165,219,80,66,141,212,117,104,9]}
```

## Interests and Content Objects

```
*Parser> gen_interest 3 (mkStdGen 42)
Interest (Name {components = [NameComponent {value = "poyvibqxfn"},NameComponent {value = "oyvibq"},NameComponent {value = "yvi"}]})

*Parser> gen_content 3 10 (mkStdGen 42)
Content (Name {components = [NameComponent {value = "poyvibqxfn"},NameComponent {value = "oyvibq"},NameComponent {value = "yvi"}]},Payload {bytes = [37,165,219,80,66,141,212,117,104,9]})
```
