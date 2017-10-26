# How-To Manage DB Migrations

We use [Liquibase] to manage the Database Schema over time.  
The integration is done with [Maven Liquibase Plugin] and you can issue commands with 
the `mvn liquibase:<command>` in the `db-migrations` directory.  

Following the [best practices] document we have main changelog file at `src/main/resources/db/changelog.yaml`. 
This file has `include` statement(s) in the correct order to the `migrations` directory where are the changelog files by major release.  
The _master_ file at `src/main/resources/db/migrations/changelog-master.yaml` has all the change sets for the initial schema creation. 


## Setting up the connection

The Liquibase plug-in settings are expected to be stored in `db-migrations/src/main/resources/liquibase.properties`.
If the file does not exists copy it from the template provided at `db-migrations/src/main/resources/liquibase.properties.template`   
Common properties are:

```properties
verbose = true
driver = com.mysql.jdbc.Driver
url = jdbc:mysql://localhost:3306/comms_router_core
username = {USERNAME}
password = {PASSWORD}
```
You should edit the file appropriately with properties required for the command you execute.

## Update

Updates the database to the latest migration.  
Required by [`liquibase:update`][liquibase:update] is the `changeLogFile` property with the path to the changelog file.

```properties
changeLogFile = src/main/resources/db/changelog.yaml
```

After adding the property issue this command:

```bash
mvn liquibase:update
```

### Create diff with changes

That will generate changeSet(s) with the difference between two given databases.

* Prerequisite for this task is to have an identical database with the new changes.

    ```sql
    CREATE DATABASE `comms_router_dev` CHARACTER SET `utf8` COLLATE `utf8_general_ci`;
    ```
    
    ```mysql
    CREATE USER IF NOT EXISTS 'comms_router'@'localhost' IDENTIFIED BY 'comms_password';
    GRANT ALL ON `comms_router_dev`.* TO 'comms_router'@'localhost';
    ```

Required by [`liquibase:diff`][liquibase:diff] are the reference arguments:

```properties
referenceDriver=com.mysql.jdbc.Driver
referenceUrl=jdbc:mysql://localhost:3306/comms_router_core
referenceUsername=comms_router
referencePassword=comms_password
```

Add the `diffChangeLogFile` property with file where the changeSet(s) will be saved:

```properties
diffChangeLogFile=src/main/resources/db/migrations/changelog-next.yaml
```

Edit the standard connection settings to point to the _dev_ database instance:

```properties
url=jdbc:mysql://localhost:3306/comms_router_dev
```

Finally execute:

```bash
mvn liquibase:diff
```

## Generate ChangeLog

To generate a changelog from existing database.

Required by [`liquibase:generateChangeLog`][liquibase:generateChangeLog] is the `outputChangeLogFile` property where 
the schema from the current database will be imported.

```properties
outputChangeLogFile	 = src/main/resources/db/changelog.yaml
```

Execute:

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

