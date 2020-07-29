CREATE TABLE "wallet_dlc_cet_sigs" ("id" SERIAL UNIQUE,"eventId" TEXT NOT NULL,"outcomeHash" TEXT NOT NULL,"signature" TEXT NOT NULL,constraint "pk_dlc_cet_sigs" primary key("eventId","outcomeHash"),constraint "fk_eventId" foreign key("eventId") references "wallet_dlcs"("eventId") on update NO ACTION on delete NO ACTION);

-- This block drops the signature columns from wallet_dlc_accepts
ALTER TABLE "wallet_dlc_accepts" DROP COLUMN "winSig", DROP COLUMN "loseSig";

-- This block drops the signature columns from wallet_dlcs
ALTER TABLE "wallet_dlcs" DROP COLUMN "initiatorWinSig", DROP COLUMN "initiatorLoseSig";
