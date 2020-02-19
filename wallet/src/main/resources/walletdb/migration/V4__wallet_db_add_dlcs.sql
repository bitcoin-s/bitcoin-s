CREATE TABLE "wallet_dlcs" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"isInitiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "keyIndex" INTEGER NOT NULL, "initiatorCetSigs" VARCHAR(254), "fundingSigs" VARCHAR(254), "oracleSig" VARCHAR(254), "dlc_offer" INTEGER, "dlc_accept" INTEGER, constraint "fk_offer" foreign key("dlc_offer") references "wallet_dlc_offers"("id"),constraint "fk_accept" foreign key("dlc_accept") references "wallet_dlc_accepts"("id"));
CREATE INDEX "wallet_dlcs_eventId_index" on "wallet_dlcs" ("eventId");

CREATE TABLE "wallet_dlc_offers" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"network" VARCHAR(254) NOT NULL,"oracleInfo" VARCHAR(254) NOT NULL,"contractInfo" VARCHAR(254) NOT NULL,"timeouts" VARCHAR(254) NOT NULL,"pubKeys" VARCHAR(254) NOT NULL,"totalCollateral" INTEGER NOT NULL,"fundingInputs" VARCHAR(254) NOT NULL,"feeRate" VARCHAR(254),"changeAddress" VARCHAR(254) NOT NULL);
CREATE INDEX "wallet_dlc_offers_eventId_index" on "wallet_dlc_offers" ("eventId");

CREATE TABLE "wallet_dlc_accepts" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"eventId" VARCHAR(254) NOT NULL UNIQUE,"pubKeys" VARCHAR(254) NOT NULL,"totalCollateral" INTEGER NOT NULL,"fundingInputs" VARCHAR(254) NOT NULL,"cetSigs" VARCHAR(254) NOT NULL,"changeAddress" VARCHAR(254) NOT NULL);
CREATE INDEX "wallet_dlc_accepts_eventId_index" on "wallet_dlc_accepts" ("eventId");
