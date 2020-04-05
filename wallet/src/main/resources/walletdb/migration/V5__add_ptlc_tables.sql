CREATE TABLE "wallet_ptlcs" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"invoiceId" VARCHAR(254) NOT NULL UNIQUE,"network" VARCHAR(254) NOT NULL,"isInitiator" INTEGER NOT NULL, "account" VARCHAR(254) NOT NULL, "keyIndex" INTEGER NOT NULL,"refundSigOpt" VARCHAR(254));
CREATE INDEX "wallet_ptlcs_invoiceId_index" on "wallet_ptlcs" ("invoiceId");

CREATE TABLE "wallet_ptlc_invoices" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"invoiceId" VARCHAR(254) NOT NULL UNIQUE,"adaptorPoint" VARCHAR(254) NOT NULL,"amount" INTEGER NOT NULL,"pubkey" VARCHAR(254) NOT NULL,"finalAddress" VARCHAR(254) NOT NULL,"timeout" INTEGER NOT NULL,constraint "fk_invoiceId" foreign key("invoiceId") references "wallet_ptlcs"("invoiceId") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_ptlc_invoices_invoiceId_index" on "wallet_ptlc_invoices" ("invoiceId");

CREATE TABLE "wallet_ptlc_accepts" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"invoiceId" VARCHAR(254) NOT NULL UNIQUE,"pubkey" VARCHAR(254) NOT NULL,"unsignedTx" VARCHAR(254) NOT NULL,"adaptorSignature" VARCHAR(254) NOT NULL,"refundAddress" VARCHAR(254) NOT NULL, constraint "fk_invoiceId" foreign key("invoiceId") references "wallet_ptlcs"("invoiceId") on update NO ACTION on delete NO ACTION);
CREATE INDEX "wallet_ptlc_accepts_invoiceId_index" on "wallet_ptlc_accepts" ("invoiceId");

CREATE TABLE "wallet_ptlc_funding_inputs" ("id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,"invoiceId" VARCHAR(254) NOT NULL,"outPoint" VARCHAR(254) NOT NULL UNIQUE, constraint "fk_invoiceId" foreign key("invoiceId") references "wallet_ptlcs"("invoiceId") on update NO ACTION on delete NO ACTION);