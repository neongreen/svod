#!/bin/bash
#
# Preparation of PostgreSQL database for testing.

set -e

SVOD_DB_NAME="svod_test"
SVOD_USER_NAME=$SVOD_DB_NAME
SVOD_PASSWORD=$SVOD_USER_NAME

### Create new database

createdb -U postgres "$SVOD_DB_NAME" 2> /dev/null || true

### Create user and grant access to the new database

psql template1 -U postgres > /dev/null <<EOF
DO
\$do\$
BEGIN
  IF NOT EXISTS
     (SELECT * FROM pg_catalog.pg_user WHERE usename = '$SVOD_USER_NAME')
    THEN CREATE USER $SVOD_USER_NAME WITH PASSWORD '$SVOD_PASSWORD';
    END IF;
END
\$do\$;

GRANT ALL PRIVILEGES ON DATABASE $SVOD_DB_NAME to $SVOD_USER_NAME;
EOF
