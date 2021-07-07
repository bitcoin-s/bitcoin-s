CREATE TABLE "contract_templates"
(
    "label"               TEXT PRIMARY KEY,
    "contract_descriptor" TEXT   NOT NULL,
    "total_collateral"    BIGINT NOT NULL
);
