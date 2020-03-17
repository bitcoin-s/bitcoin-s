CREATE TABLE "wallet_dlc_cet_sigs" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL,"outcomeHash" VARCHAR(254) NOT NULL,"signature" VARCHAR(254) NOT NULL,constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);

-- This block drops the signature columns from wallet_dlc_accepts
CREATE TEMPORARY TABLE "wallet_dlc_accepts_backup" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"fundingKey" VARCHAR(254) NOT NULL,"toLocalCETKey" VARCHAR(254) NOT NULL,"finalAddress" VARCHAR(254) NOT NULL,"totalCollateral" INTEGER NOT NULL,"refundSig" VARCHAR(254) NOT NULL,"changeAddress" VARCHAR(254) NOT NULL, constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_dlc_accepts_backup" SELECT "id", "eventId", "fundingKey", "toLocalCETKey", "finalAddress", "totalCollateral", "refundSig", "changeAddress" FROM "wallet_dlc_accepts";
DROP TABLE "wallet_dlc_accepts";
CREATE TABLE "wallet_dlc_accepts" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"fundingKey" VARCHAR(254) NOT NULL,"toLocalCETKey" VARCHAR(254) NOT NULL,"finalAddress" VARCHAR(254) NOT NULL,"totalCollateral" INTEGER NOT NULL,"refundSig" VARCHAR(254) NOT NULL,"changeAddress" VARCHAR(254) NOT NULL, constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_dlc_accepts" SELECT "id", "eventId", "fundingKey", "toLocalCETKey", "finalAddress", "totalCollateral", "refundSig", "changeAddress" FROM "wallet_dlc_accepts_backup";
DROP TABLE "wallet_dlc_accepts_backup";

-- This block drops the signature columns from wallet_dlcs
CREATE TEMPORARY TABLE "wallet_dlcs_backup" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"isInitiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "keyIndex" INTEGER NOT NULL,"initiatorRefundSig" VARCHAR(254),"oracleSig" VARCHAR(254));
INSERT INTO "wallet_dlcs_backup" SELECT "id", "eventId", "isInitiator", "account", "keyIndex", "initiatorRefundSig", "oracleSig" FROM "wallet_dlcs";
DROP TABLE "wallet_dlcs";
CREATE TABLE "wallet_dlcs" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"isInitiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "keyIndex" INTEGER NOT NULL,"initiatorRefundSig" VARCHAR(254),"oracleSig" VARCHAR(254));
INSERT INTO "wallet_dlcs" SELECT "id", "eventId", "isInitiator", "account", "keyIndex", "initiatorRefundSig", "oracleSig" FROM "wallet_dlcs_backup";
DROP TABLE "wallet_dlcs_backup";
