Haskell Bittorrent
==========

Currently a work in progress. 

Done:

- Bencode parser: able to get information from torrent files, and have successfully gotten correct info_hashes
- Distinguish between single file and multi file torrents.


To Do:

- Make a request to tracker
- Handshake with a peer (to start with)
- Connect to multiple peers with multi-threading or async