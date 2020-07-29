CREATE TABLE "wallet_dlcs" ("event_id" TEXT NOT NULL UNIQUE,"state" TEXT NOT NULL,"is_initiator" BOOLEAN NOT NULL, "account" TEXT NOT NULL, "key_index" INTEGER NOT NULL,"oracle_sig" TEXT, constraint "pk_dlc" primary key("event_id"));
CREATE INDEX "wallet_dlcs_event_id_index" on "wallet_dlcs" ("event_id");

CREATE TABLE "wallet_dlc_offers" ("oracle_pub_key" TEXT NOT NULL,"oracle_r_value" TEXT NOT NULL,"contract_info" TEXT NOT NULL,"contract_maturity" TEXT NOT NULL,"contract_timeout" TEXT NOT NULL,"funding_key" TEXT NOT NULL,"payout_address" TEXT NOT NULL,"total_collateral" INTEGER NOT NULL,"fee_rate" TEXT,"change_address" TEXT NOT NULL,constraint "pk_dlc_offer" primary key("event_id"),constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id")on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_offers_event_id_index" on "wallet_dlc_offers" ("event_id");

CREATE TABLE "wallet_dlc_accepts" ("funding_key" TEXT NOT NULL,"payout_address" TEXT NOT NULL,"total_collateral" INTEGER NOT NULL,"change_address" TEXT NOT NULL,constraint "pk_dlc_accept" primary key("event_id"), constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_accepts_event_id_index" on "wallet_dlc_accepts" ("event_id");

CREATE TABLE "wallet_dlc_funding_inputs" ("is_initiator" BOOLEAN NOT NULL,"out_point" TEXT NOT NULL UNIQUE,"output" TEXT NOT NULL,"redeem_script_opt" TEXT, "witness_script_opt" TEXT,"sigs" TEXT,constraint "pk_dlc_input" primary key("out_point"), constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_cet_sigs" ("event_id" TEXT NOT NULL,"outcome_hash" TEXT NOT NULL,"signature" TEXT NOT NULL,constraint "pk_dlc_cet_sigs" primary key("event_id","outcome_hash"),constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_refund_sigs" ("event_id" TEXT NOT NULL,"is_initiator" INTEGER NOT NULL,"refund_sig" TEXT NOT NULL,constraint "fk_event_id" foreign key("event_id") references "wallet_dlcs"("event_id") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_refund_sigs_event_id_index" on "wallet_dlc_accepts" ("event_id");
