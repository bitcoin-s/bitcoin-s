CREATE TABLE "wallet_dlcs" ("param_hash" VARCHAR(254) NOT NULL UNIQUE, "temp_contract_id" VARCHAR(254) UNIQUE,"contract_id" VARCHAR(254) UNIQUE,"state" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "key_index" INTEGER NOT NULL, "oracle_sig" VARCHAR(254), "funding_tx_id" VARCHAR(254), "closing_tx_id" VARCHAR(254));
CREATE INDEX "wallet_dlcs_param_hash_index" on "wallet_dlcs" ("param_hash");

CREATE TABLE "wallet_dlc_offers" ("param_hash" VARCHAR(254) NOT NULL UNIQUE,"oracle_pub_key" VARCHAR(254) NOT NULL,"oracle_r_value" VARCHAR(254) NOT NULL,"contract_info" VARCHAR(254) NOT NULL,"contract_maturity" VARCHAR(254) NOT NULL,"contract_timeout" VARCHAR(254) NOT NULL,"funding_key" VARCHAR(254) NOT NULL,"payout_address" VARCHAR(254) NOT NULL,"total_collateral" INTEGER NOT NULL,"fee_rate" VARCHAR(254),"change_address" VARCHAR(254) NOT NULL,constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash")on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_offers_param_hash_index" on "wallet_dlc_offers" ("param_hash");

CREATE TABLE "wallet_dlc_accepts" ("param_hash" VARCHAR(254) NOT NULL UNIQUE,"funding_key" VARCHAR(254) NOT NULL,"payout_address" VARCHAR(254) NOT NULL,"total_collateral" INTEGER NOT NULL,"change_address" VARCHAR(254) NOT NULL, constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_accepts_param_hash_index" on "wallet_dlc_accepts" ("param_hash");

CREATE TABLE "wallet_dlc_funding_inputs" ("param_hash" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL,"out_point" VARCHAR(254) NOT NULL UNIQUE,"output" VARCHAR(254) NOT NULL,"redeem_script_opt" VARCHAR(254), "witness_script_opt" VARCHAR(254), "sigs" VARCHAR(254), constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_cet_sigs" ("param_hash" VARCHAR(254) NOT NULL,"outcome_hash" VARCHAR(254) NOT NULL,"signature" VARCHAR(254) NOT NULL,constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_refund_sigs" ("param_hash" VARCHAR(254) NOT NULL,"is_initiator" INTEGER NOT NULL,"refund_sig" VARCHAR(254) NOT NULL,constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_refund_sigs_param_hash_index" on "wallet_dlc_accepts" ("param_hash");
