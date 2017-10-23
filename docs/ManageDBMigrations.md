# How-To Manage DB Migrations

We use [Liquibase] to manage the Database Schema over time.  
The integration is done with [Maven Liquibase Plugin] and you can issue commands with 
the `mvn liquibase:<command>` in the `web` directory.  

Following the [best practices] document we have main changelog file at `src/main/resources/db/changelog.yaml`. 
This file has `include` statements in the correct order to the `migrations` directory where are the changelog files by major release.  
The _master_ file at `src/main/resources/db/migrations/changelog-master.yaml` has all the change sets for the initial schema creation. 


## Setting up the connection

The Maven Plug-in settings are stored in `web/src/main/resources/liquibase.properties`.  
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

## Generate ChangeLog

To generate a changelog from existing database.

Required by [`liquibase:generateChangeLog`][liquibase:generateChangeLog] is the `outputChangeLogFile` property where 
the schema from the current database will be imported.

```properties
outputChangeLogFile	 = src/main/resources/db/changelog.yaml
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

[liquibase:generateChangeLog]: 
http://www.liquibase.org/documentation/maven/maven_generateChangeLog.html 
"Maven generateChangeLog"

