CREATE TABLE incoming_offers (hash VARCHAR(64) PRIMARY KEY, received_at TIMESTAMP NOT NULL, peer VARCHAR(1024), message VARCHAR(280), offer_tlv TEXT NOT NULL);

CREATE INDEX incoming_offers_peer_idx ON incoming_offers(peer);

CREATE INDEX incoming_offers_received_at_idx ON incoming_offers(received_at);
