Haskell Bittorrent
==========

Currently on hiatus. The project was wearing on me.

What Works
---------
At this point, this BitTorrent client is capable of parsing a bencoded torrent file, extracting the necessary information needed for connecting to a tracker (SHA1 hash of the info dictionary value, the announce URL, and length of the torrent in kbs). It can connect to a tracker, parse peer data, and correctly handshake with them. Messages can be received from multiple peers, and are parsed on a case by case basis (there are about 11 different messages that can be received or sent).

What Doesn't Work and Thoughts
----------
Currently this BitTorrent client is not responding to peers based on messages. This is where I have decided to stop for the time being. The architecture for actually sending a message is in place. I just need to process each message from a peer and act accordingly to their message. I also will need to work on writing to file. I do not plan on working on uploading to peers. This project was merely for me to understand some networking concepts and the BitTorrent protocol. Writing this in Haskell also allowed me to use some Monads to preserve state and learn about how to do "unpure" things in a functional language.


Checklist
-----

- [x] Bencode parser: able to get information from torrent files, and have successfully gotten correct info_hashes
- [x] Distinguish between single file and multi file torrents.
- [x] Make a request to tracker (get back response, response body includes bencoded dictionary of necessary info to connect to peers and make requests to tracker)
- [x] Move ```parseBinaryModel``` to Peers.hs
- [x] Handshake with a peer
- [x] Parse peer handshake
- [x] Create data type for BitField
- [x] Connect to multiple peers with multi-threading or async
- [ ] Refactor / modularize code

Other notes
-----
This was a project started at my time at Hacker School (Summer '14 batch). Hopefully I will finish it at some point.