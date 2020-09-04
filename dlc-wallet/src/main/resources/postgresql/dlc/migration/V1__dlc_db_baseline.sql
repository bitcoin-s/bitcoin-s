CREATE TABLE "wallet_dlcs" ("param_hash" TEXT NOT NULL UNIQUE,"temp_contract_id" TEXT UNIQUE,"contract_id" TEXT UNIQUE,"state" TEXT NOT NULL,"is_initiator" BOOLEAN NOT NULL, "account" TEXT NOT NULL, "key_index" INTEGER NOT NULL,"oracle_sig" TEXT,"funding_tx_id" TEXT,"closing_tx_id" TEXT, constraint "pk_dlc" primary key("param_hash"));
CREATE INDEX "wallet_dlcs_param_hash_index" on "wallet_dlcs" ("param_hash");

CREATE TABLE "wallet_dlc_offers" ("param_hash" VARCHAR(254) NOT NULL UNIQUE,"oracle_pub_key" TEXT NOT NULL,"oracle_r_value" TEXT NOT NULL,"contract_info" TEXT NOT NULL,"contract_maturity" TEXT NOT NULL,"contract_timeout" TEXT NOT NULL,"funding_key" TEXT NOT NULL,"payout_address" TEXT NOT NULL,"total_collateral" INTEGER NOT NULL,"fee_rate" TEXT,"change_address" TEXT NOT NULL,constraint "pk_dlc_offer" primary key("param_hash"),constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash")on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_offers_param_hash_index" on "wallet_dlc_offers" ("param_hash");

CREATE TABLE "wallet_dlc_accepts" ("param_hash" VARCHAR(254) NOT NULL UNIQUE,"funding_key" TEXT NOT NULL,"payout_address" TEXT NOT NULL,"total_collateral" INTEGER NOT NULL,"change_address" TEXT NOT NULL,constraint "pk_dlc_accept" primary key("param_hash"), constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_accepts_param_hash_index" on "wallet_dlc_accepts" ("param_hash");

CREATE TABLE "wallet_dlc_funding_inputs" ("param_hash" VARCHAR(254) NOT NULL UNIQUE,"is_initiator" BOOLEAN NOT NULL,"out_point" TEXT NOT NULL UNIQUE,"output" TEXT NOT NULL,"redeem_script_opt" TEXT, "witness_script_opt" TEXT,"sigs" TEXT,constraint "pk_dlc_input" primary key("out_point"), constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_cet_sigs" ("param_hash" TEXT NOT NULL,"outcome_hash" TEXT NOT NULL,"signature" TEXT NOT NULL,constraint "pk_dlc_cet_sigs" primary key("param_hash","outcome_hash"),constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);

CREATE TABLE "wallet_dlc_refund_sigs" ("param_hash" TEXT NOT NULL,"is_initiator" INTEGER NOT NULL,"refund_sig" TEXT NOT NULL,constraint "fk_param_hash" foreign key("param_hash") references "wallet_dlcs"("param_hash") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_dlc_refund_sigs_param_hash_index" on "wallet_dlc_accepts" ("param_hash");
