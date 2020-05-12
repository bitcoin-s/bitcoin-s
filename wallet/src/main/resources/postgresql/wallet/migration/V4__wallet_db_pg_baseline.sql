CREATE TABLE wallet_accounts (hd_purpose INTEGER NOT NULL,xpub TEXT NOT NULL,coin INTEGER NOT NULL,account_index INTEGER NOT NULL,constraint pk_account primary key(hd_purpose,coin,account_index));

CREATE TABLE addresses (hd_purpose INTEGER NOT NULL,account_index INTEGER NOT NULL,hd_coin INTEGER NOT NULL,hd_chain_type INTEGER NOT NULL,address TEXT PRIMARY KEY NOT NULL,script_witness TEXT,script_pub_key TEXT NOT NULL UNIQUE,address_index INTEGER NOT NULL,pubkey TEXT NOT NULL,hashed_pubkey TEXT NOT NULL,script_type TEXT NOT NULL,constraint fk_account foreign key (hd_purpose,hd_coin,account_index) references wallet_accounts(hd_purpose,coin,account_index));

CREATE TABLE tx_table (id SERIAL UNIQUE, "txIdBE" TEXT NOT NULL,transaction TEXT NOT NULL,"unsignedTxIdBE" TEXT NOT NULL,"unsignedTx" TEXT NOT NULL,"wTxIdBE" TEXT,"totalOutput" BIGINT NOT NULL,"numInputs" INTEGER NOT NULL,"numOutputs" INTEGER NOT NULL,locktime BIGINT NOT NULL, constraint pk_tx primary key ("txIdBE"));

CREATE TABLE wallet_incoming_txs (id SERIAL UNIQUE,"txIdBE" TEXT NOT NULL,"incomingAmount" BIGINT NOT NULL,constraint fk_underlying_tx foreign key("txIdBE") references tx_table("txIdBE") on update NO ACTION on delete NO ACTION, constraint pk_in_tx primary key ("txIdBE"));

CREATE TABLE wallet_outgoing_txs (id SERIAL UNIQUE,"txIdBE" TEXT NOT NULL,"inputAmount" BIGINT NOT NULL,"sentAmount" BIGINT NOT NULL,"actualFee" BIGINT NOT NULL,"expectedFee" BIGINT NOT NULL,"feeRate" BIGINT NOT NULL,constraint fk_underlying_tx foreign key("txIdBE") references tx_table("txIdBE") on update NO ACTION on delete NO ACTION, constraint pk_out_tx primary key ("txIdBE"));

CREATE TABLE txo_spending_info (id SERIAL PRIMARY KEY,tx_outpoint TEXT NOT NULL, script_pub_key TEXT NOT NULL,value BIGINT NOT NULL,hd_privkey_path TEXT NOT NULL,redeem_script TEXT,script_witness TEXT,txid TEXT NOT NULL,block_hash TEXT, txo_state TEXT NOT NULL, constraint fk_scriptPubKey foreign key(script_pub_key) references addresses(script_pub_key), constraint fk_incoming_txId foreign key(txid) references wallet_incoming_txs("txIdBE") on update NO ACTION on delete NO ACTION);
