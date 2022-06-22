drop table "peers";

create table "peers" ("address" VARCHAR NOT NULL, "port" INT NOT NULL, "last_seen" TIMESTAMP,"first_seen" TIMESTAMP, "network_id" INT, "service_bytes" VARCHAR, constraint pk_peers PRIMARY KEY (address,port));