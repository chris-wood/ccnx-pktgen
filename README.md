# Overview

ccnx-pktgen is a CCNx packet grammar parser and generator. The tool is meant to be a general
purpose packet generator for tests and experiments. It is also intended to 
guide packet format experimentation. 

# Grammar Specification Notes

The current CCNx packet grammar is specified online at:

https://tools.ietf.org/html/draft-irtf-icnrg-ccnxmessages-00#section-3.6.1

The grammar has been extracted and translated to its ABNF representation
here. The following grammar is used internally within ccnx-pktgen to 
determine how packets are to be constructed. Terminal values in the grammar
are mapped to data sources from which values can be randomly generated. 
For example, consider the following grammar:

~~~
T_NAME := G_NAME | T_NAME_SEGMENT+
T_NAME_SEGMENT := G_NAME_SEGMENT
~~~

This says that a TLV of type T_NAME can be either a G_NAME or
a list of one or more T_NAME_SEGMENTS. The TLV T_NAME_SEGMENT
is a terminal value. Both G_NAME and G_NAME_SEGMENT are generators
from which actual values can be sampled. By convention, the grammar
will be written such that all TLV types are prefixed with "T_"
and all terminal generators are prefixed with "G_".

# CCNx Packet Grammar

T_NAME := G_NAME | T_NAME_SEGMENT+
T_NAME_SEGMENT := G_NAME_SEGMENT


