CREATE TABLE "dlc_contact_mapping" (
    "dlc_id" TEXT NOT NULL PRIMARY KEY,
    "contact_id" TEXT NOT NULL,
    CONSTRAINT "fk_dlc_contact_dlc_id" FOREIGN KEY ("dlc_id") references "global_dlc_data" ("dlc_id") on update NO ACTION on delete NO ACTION,
    CONSTRAINT "fk_dlc_contact_contact_id" FOREIGN KEY ("contact_id") references "contacts" ("address") on update NO ACTION on delete NO ACTION
);