create table if not exists "peers" ("address" VARCHAR NOT NULL PRIMARY KEY UNIQUE,"port" INT NOT NULL,"last_seen" TIMESTAMP,"first_seen" TIMESTAMP, "network_id" INT)
