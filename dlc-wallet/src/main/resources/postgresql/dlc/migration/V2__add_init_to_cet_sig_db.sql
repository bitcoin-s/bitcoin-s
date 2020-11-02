ALTER TABLE "wallet_dlc_cet_sigs" ADD COLUMN is_initiator BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE "wallet_dlcs" ADD COLUMN "funding_outpoint" VARCHAR(254);
