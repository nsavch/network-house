network-house
=============

The network stack extracted from the [House](https://code.google.com/p/pdxhouse/) project.

Provides data structures and parsers for Ethernet, TCP, UDP, IPv4, IPv6, ICMP, DHCP and TFTP packets as well as some server implementations.

This fork uses ByteString instead of UArray to increase performance of parsing networking packets. I didn't make precise benchmarks so far, but roughly speaking it lowers CPU usage 5 times according to my observations.

This package was extracted from house due to the lack of existing network package parsing libraries.

License: GPLv2 (from House)
