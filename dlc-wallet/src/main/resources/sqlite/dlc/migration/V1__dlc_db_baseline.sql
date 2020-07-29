CREATE TABLE "wallet_dlcs" ("event_id" VARCHAR(254) NOT NULL UNIQUE,"state" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "key_index" INTEGER NOT NULL, "oracle_sig" VARCHAR(254), "funding_tx_id" VARCHAR(254), "closing_tx_id" VARCHAR(254));
CREATE INDEX "wallet_dlcs_event_id_index" on "wallet_dlcs" ("event_id");

CREATE TABLE "wallet_dlc_offers" ("event_id" VARCHAR(254) NOT NULL UNIQUE,"oracle_pub_key" VARCHAR(254) NOT NULL,"oracle_r_value" VARCHAR(254) NOT NULL,"contract_info" VARCHAR(254) NOT NULL,"contract_maturity" VARCHAR(254) NOT NULL,"contract_timeout" VARCHAR(254) NOT NULL,"funding_key" VARCHAR(254) NOT NULL,"payout_address" VARCHAR(254) NOT NULL,"total_collateral" INTEGER NOT NULL,"fee_rate" VARCHAR(254),"change_address" VARCHAR(254) NOT NULL,constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id")on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_offers_event_id_index" on "wallet_dlc_offers" ("event_id");

CREATE TABLE "wallet_dlc_accepts" ("event_id" VARCHAR(254) NOT NULL UNIQUE,"funding_key" VARCHAR(254) NOT NULL,"payout_address" VARCHAR(254) NOT NULL,"total_collateral" INTEGER NOT NULL,"change_address" VARCHAR(254) NOT NULL, constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_accepts_event_id_index" on "wallet_dlc_accepts" ("event_id");

CREATE TABLE "wallet_dlc_funding_inputs" ("event_id" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL,"out_point" VARCHAR(254) NOT NULL UNIQUE,"output" VARCHAR(254) NOT NULL,"redeem_script_opt" VARCHAR(254), "witness_script_opt" VARCHAR(254), "sigs" VARCHAR(254), constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_cet_sigs" ("event_id" VARCHAR(254) NOT NULL,"outcome_hash" VARCHAR(254) NOT NULL,"signature" VARCHAR(254) NOT NULL,constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_refund_sigs" ("event_id" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL,"refund_sig" VARCHAR(254) NOT NULL,constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_refund_sigs_event_id_index" on "wallet_dlc_accepts" ("event_id");
