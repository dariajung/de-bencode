Haskell Bittorrent
==========

Currently a work in progress. 

Done:

- Bencode parser: able to get information from torrent files, and have successfully gotten correct info_hashes
- Distinguish between single file and multi file torrents.
- Make a request to tracker (get back response, response body includes bencoded dictionary of necessary info to connect to peers and make requests to tracker)

To Do:

- Move ```parseBinaryModel``` to Peers.hs
- Handshake with a peer (to start with)
- Connect to multiple peers with multi-threading or async
