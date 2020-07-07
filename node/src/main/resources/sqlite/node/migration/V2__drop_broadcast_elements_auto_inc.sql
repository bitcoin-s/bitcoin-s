-- This block drops the "id" column
CREATE TEMPORARY TABLE "broadcast_elements_backup" ("txid" VARCHAR(254) NOT NULL UNIQUE,"tx_bytes" VARCHAR(254) NOT NULL);
INSERT INTO "broadcast_elements_backup" SELECT "txid", "tx_bytes" FROM "broadcast_elements";
DROP TABLE "broadcast_elements";
CREATE TABLE "broadcast_elements" ("txid" VARCHAR(254) PRIMARY KEY,"tx_bytes" VARCHAR(254) NOT NULL);
INSERT INTO "broadcast_elements" SELECT "txid", "tx_bytes" FROM "broadcast_elements";
DROP TABLE "broadcast_elements_backup";