CREATE TABLE "contract_templates"
(
    "label"               VARCHAR(254) PRIMARY KEY,
    "contract_descriptor" VARCHAR(254) NOT NULL,
    "total_collateral"    BIGINT       NOT NULL
);
