-- This changes the primary key to (address, tag_type)
CREATE TABLE "wallet_address_tags_temp" ("address" VARCHAR(254) NOT NULL,"tag_name" VARCHAR(254) NOT NULL,"tag_type" VARCHAR(254) NOT NULL);
INSERT INTO "wallet_address_tags_temp" SELECT "address", "tag_name", "tag_type" FROM "wallet_address_tags";
DROP TABLE "wallet_address_tags";
CREATE TABLE "wallet_address_tags" ("address" VARCHAR(254) NOT NULL,"tag_name" VARCHAR(254) NOT NULL,"tag_type" VARCHAR(254) NOT NULL,constraint "pk_address_tags" primary key ("address", "tag_type"), constraint "fk_address" foreign key("address") references "addresses"("address") on update NO ACTION on delete NO ACTION);
INSERT INTO "wallet_address_tags" SELECT "address", "tag_name", "tag_type" FROM "wallet_address_tags";
DROP TABLE "wallet_address_tags_temp";
