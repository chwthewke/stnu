CREATE TABLE "plans"
( "id"       SERIAL       NOT NULL  
, "name"     TEXT         NOT NULL
, "updated"  TIMESTAMPTZ  NOT NULL

, CONSTRAINT "plans_pk" PRIMARY KEY ("id")
, CONSTRAINT "plans_name_unique" UNIQUE ("name")
);

CREATE TABLE "plan_options"
( "plan_id"             INTEGER       NOT NULL
, "hide_ficsmas"        BOOLEAN       NOT NULL
, "miner_class"         VARCHAR(256)  NOT NULL
, "clock_speed_preset"  VARCHAR(16)   NOT NULL

, CONSTRAINT "plan_options_plan_fk" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id") ON DELETE CASCADE
, CONSTRAINT "plan_options_plan_unique" UNIQUE ("plan_id")
);

CREATE TYPE "plan_allowed_t" AS ENUM
( 'recipe' 
, 'extractor_type'
, 'belt'
, 'pipeline'
, 'generator'
);

CREATE TABLE "plan_allowed_classes"
( "plan_id"  INTEGER           NOT NULL
, "type"     "plan_allowed_t"  NOT NULL
, "class"    VARCHAR(256)      NOT NULL
, "single"   BOOLEAN           NOT NULL

, CONSTRAINT "plan_allowed_classes_plan_fk" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id") ON DELETE CASCADE
, CONSTRAINT "plan_allowed_classes_unique" UNIQUE ("plan_id", "type", "class")
);

CREATE TYPE "purity_t" AS ENUM
( 'impure'
, 'normal'
, 'pure'
);

CREATE TABLE "plan_resource_nodes"
( "plan_id"         INTEGER       NOT NULL
, "extractor_type"  VARCHAR(64)   NOT NULL
, "item_class"      VARCHAR(256)  NOT NULL
, "purity"          "purity_t"    NOT NULL
, "amount"          INTEGER       NOT NULL

, CONSTRAINT "plan_resource_nodes_plan_fk" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id") ON DELETE CASCADE
, CONSTRAINT "plan_resource_node_unique" UNIQUE ("plan_id", "extractor_type", "item_class", "purity")
);

CREATE TABLE "plan_resource_options"
( "plan_id"          INTEGER       NOT NULL
, "item_class"       VARCHAR(256)  NOT NULL
, "prefer_fracking"  BOOLEAN       NOT NULL
, "value"            INTEGER       NOT NULL

, CONSTRAINT "plan_resource_options_plan_fk" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id") ON DELETE CASCADE
, CONSTRAINT "plan_resource_options_unique" UNIQUE ("plan_id", "item_class")
);

CREATE TABLE "plan_requested"
( "plan_id"     INTEGER           NOT NULL
, "item_class"  VARCHAR(256)      NOT NULL
, "amount"      DOUBLE PRECISION  NOT NULL

, CONSTRAINT "plan_requested_plan_fk" FOREIGN KEY ("plan_id") REFERENCES "plans" ("id") ON DELETE CASCADE
, CONSTRAINT "plan_requested_unique" UNIQUE ("plan_id", "item_class")
);

CREATE INDEX "plan_options_plan_index" ON "plan_options" ("plan_id"); 
CREATE INDEX "plan_allowed_classes_plan_index" ON "plan_allowed_classes" ("plan_id"); 
CREATE INDEX "plan_resource_nodes_plan_index" ON "plan_resource_nodes" ("plan_id"); 
CREATE INDEX "plan_resource_options_plan_index" ON "plan_resource_options" ("plan_id"); 
CREATE INDEX "plan_requested_plan_index" ON "plan_requested" ("plan_id"); 
