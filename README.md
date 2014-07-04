Haskell Bittorrent
==========

Currently a work in progress. 

- [x] Bencode parser: able to get information from torrent files, and have successfully gotten correct info_hashes
- [x] Distinguish between single file and multi file torrents.
- [x] Make a request to tracker (get back response, response body includes bencoded dictionary of necessary info to connect to peers and make requests to tracker)
- [x] Move ```parseBinaryModel``` to Peers.hs
- [x] Handshake with a peer
- [x] Parse peer handshake
- [ ] Create data type for BitField
- [ ] Connect to multiple peers with multi-threading or async
- [ ] Refactor / modularize code
