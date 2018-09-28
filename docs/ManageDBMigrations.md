# How to manage database migrations

We use [Liquibase] to manage the Database Schema over time. The integration is done with [Maven Liquibase Plugin] and you can issue commands with the `mvn liquibase:<command>` in the `db-migrations` directory.  

Following the [best practices] document we have main changelog file at `db-migrations/src/main/resources/db/changelog.yaml`. This file has `include` statement(s) in the correct order to the `migrations` directory where are the changelog files by major release. The _master_ file at `src/main/resources/db/migrations/changelog-master.yaml` has all the change sets for the initial schema creation. 

## Standard migratiom 

1. Setting up the connection. The Liquibase plug-in settings are expected to be stored in `db-migrations/src/main/resources/liquibase.properties`. If the file does not exists copy it from the template provided at `db-migrations/src/main/resources/liquibase.properties.template` and update the following properties.

    ```properties
    verbose = true
    driver = com.mysql.jdbc.Driver
    url = jdbc:mysql://localhost:3306/comms_router_core
    username = {USERNAME}
    password = {PASSWORD}
    ```

2. Update the database to the latest migration.

3. Check and update the `changeLogFile` property in `db-migrations/src/main/resources/liquibase.properties`. Required by [`liquibase:update`][liquibase:update].

    ```properties
	changeLogFile = src/main/resources/db/changelog.yaml
    ```

4. Update liquidbase by running the following command in the `db-migrations` directory.

    ```bash
	mvn liquibase:update
    ```

## Create diff with changes
Generate changeSet(s) with the difference between two given databases. In this example we will create an additional database names `comms_router_dev`.

**Note:** Prerequisite for this task is to have an identical database with the new changes.

1. Create new database.

    ```sql
    CREATE DATABASE `comms_router_dev` CHARACTER SET `utf8` COLLATE `utf8_general_ci`;
    ```
    
2. Create new Super Admin.

    ```mysql
    CREATE USER IF NOT EXISTS 'comms_router'@'localhost' IDENTIFIED BY 'comms_router_password';
    GRANT ALL ON `comms_router_dev`.* TO 'comms_router'@'localhost';
    ```

3. Check and update reference arguments in `db-migrations/src/main/resources/liquibase.properties`. Required by [`liquibase:diff`][liquibase:diff].

    ```properties
	referenceDriver=com.mysql.jdbc.Driver
	referenceUrl=jdbc:mysql://localhost:3306/comms_router_core
	referenceUsername=comms_router
	referencePassword=comms_password
    ```

4. Add the `diffChangeLogFile` property in `db-migrations/src/main/resources/liquibase.properties` where the changeSet(s) will be saved.

    ```properties
	diffChangeLogFile=src/main/resources/db/migrations/changelog-next.yaml
    ```

5. Edit the `url` property in `db-migrations/src/main/resources/liquibase.properties` by updating the standard connection settings to point to the _dev_ database instance.

    ```properties
	url=jdbc:mysql://localhost:3306/comms_router_dev
    ```

6. Update liquidbase by running the following command.

    ```bash
	mvn liquibase:update
    ```

## Generate ChangeLog

1. Add the `outputChangeLogFile` property in `db-migrations/src/main/resources/liquibase.properties` to where the schema from the current database will be imported from. Required by [`liquibase:generateChangeLog`][liquibase:generateChangeLog].
    
    ```properties
    outputChangeLogFile	 = src/main/resources/db/changelog.yaml
    ```

2. Update liquidbase by running the following command.

    ```bash
    mvn liquibase:generateChangeLog
    ```

[Liquibase]: 
http://www.liquibase.org 
"Liquibase"

[Maven Liquibase Plugin]: 
http://www.liquibase.org/documentation/maven/ 
"Maven Liquibase Plugin"

[best practices]:
http://www.liquibase.org/bestpractices.html
"Best practices"

[liquibase:update]: 
http://www.liquibase.org/documentation/maven/maven_update.html 
"Maven update"

[liquibase:diff]:
http://www.liquibase.org/documentation/maven/maven_diff.html
"Maven diff"

[liquibase:generateChangeLog]: 
http://www.liquibase.org/documentation/maven/maven_generateChangeLog.html 
"Maven generateChangeLog"

