ALTER TABLE "plan_options"
  DROP COLUMN "production_rows_order"
, DROP COLUMN "complete"
, ADD COLUMN "organisation" JSONB DEFAULT NULL
;
