ALTER TABLE "global_dlc_data" ADD COLUMN "peer" VARCHAR(1024);

CREATE INDEX "global_dlc_data_peer_idx" ON "global_dlc_data"("peer");
