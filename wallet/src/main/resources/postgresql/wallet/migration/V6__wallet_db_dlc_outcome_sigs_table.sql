CREATE TABLE "wallet_dlc_cet_sigs" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL,"outcomeHash" TEXT NOT NULL,"signature" TEXT NOT NULL,constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);

-- This block drops the signature columns from wallet_dlc_accepts
CREATE TEMPORARY TABLE "wallet_dlc_accepts_backup" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL UNIQUE,"fundingKey" TEXT NOT NULL,"toLocalCETKey" TEXT NOT NULL,"finalAddress" TEXT NOT NULL,"totalCollateral" INTEGER NOT NULL,"refundSig" TEXT NOT NULL,"changeAddress" TEXT NOT NULL, constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_dlc_accepts_backup" SELECT "id", "eventId", "fundingKey", "toLocalCETKey", "finalAddress", "totalCollateral", "refundSig", "changeAddress" FROM "wallet_dlc_accepts";
DROP TABLE "wallet_dlc_accepts";
CREATE TABLE "wallet_dlc_accepts" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL UNIQUE,"fundingKey" TEXT NOT NULL,"toLocalCETKey" TEXT NOT NULL,"finalAddress" TEXT NOT NULL,"totalCollateral" INTEGER NOT NULL,"refundSig" TEXT NOT NULL,"changeAddress" TEXT NOT NULL, constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_dlc_accepts" SELECT "id", "eventId", "fundingKey", "toLocalCETKey", "finalAddress", "totalCollateral", "refundSig", "changeAddress" FROM "wallet_dlc_accepts_backup";
DROP TABLE "wallet_dlc_accepts_backup";

-- This block drops the signature columns from wallet_dlcs
CREATE TEMPORARY TABLE "wallet_dlcs_backup" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL UNIQUE,"isInitiator" INTEGER NOT NULL, "account" TEXT NOT NULL, "keyIndex" INTEGER NOT NULL,"initiatorRefundSig" TEXT,"oracleSig" TEXT);
INSERT INTO "wallet_dlcs_backup" SELECT "id", "eventId", "isInitiator", "account", "keyIndex", "initiatorRefundSig", "oracleSig" FROM "wallet_dlcs";
DROP TABLE "wallet_dlcs";
CREATE TABLE "wallet_dlcs" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL UNIQUE,"isInitiator" INTEGER NOT NULL, "account" TEXT NOT NULL, "keyIndex" INTEGER NOT NULL,"initiatorRefundSig" TEXT,"oracleSig" TEXT);
INSERT INTO "wallet_dlcs" SELECT "id", "eventId", "isInitiator", "account", "keyIndex", "initiatorRefundSig", "oracleSig" FROM "wallet_dlcs_backup";
DROP TABLE "wallet_dlcs_backup";
