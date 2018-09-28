# Configuring Database Access
The application expects database access to be provided from the Application Container via JNDI.

## System requirements
* Java - Oracle JDK/JRE 8 (build/runtime)
* Apache Maven - 3.5 (build)
* SQL Server - MySQL 5.7 (runtime)
* Java Servlet Container - Tomcat 8 (runtime)

The Comms Router API may work with different types of Java, SQL Server or Web Container and is currently being tested and maintained with the above component versions.

# Configure MySQL

## Create the Database
Create the database with `CHARACTER SET utf8 COLLATE utf8_general_ci`.

```sql
CREATE DATABASE `comms_router_core` CHARACTER SET `utf8` COLLATE `utf8_general_ci`;
```

## Create Users
There are two options for setting up your Comms Router application database.
* Create two users Database Admin with full permissions (create, edit, delete) and Application Admin to manage Tasks, Agents, Queues and Plans.
* Create a Super Admin user to manage both database and content.

### Creating Database and Application Admins
#### Database Admin
```mysql
CREATE USER 'comms_router_admin'@'localhost' IDENTIFIED BY 'comms_router_admin_password';
GRANT ALL ON `comms_router_core`.* TO 'comms_router_admin'@'localhost';
```
    
#### Application Admin
```mysql
CREATE USER 'comms_router'@'localhost' IDENTIFIED BY 'comms_router_password';
GRANT LOCK TABLES, SELECT, INSERT, DELETE, UPDATE ON `comms_router_core`.* TO 'comms_router'@'localhost';
```

### Creating Super Admin
```mysql
CREATE USER 'comms_router'@'localhost' IDENTIFIED BY 'comms_router_password';
GRANT ALL ON `comms_router_core`.* TO 'comms_router'@'localhost';
```

# Configure Tomcat

1. First create `context.xml` configuration for Tomcat deployment either by updating:
   
    * `comms-router-web.xml` in `CATALINA_BASE/conf/ENGINE_NAME/HOST_NAME/` or use another file.
    * Add a resource defined in `CATALINA_BASE/conf/context.xml` or in `CATALINA_BASE/conf/ENGINE_NAME/context.xml`

    For more help read the [documention][1].

2. In the chosen file add the datasource resource definition with the recommended configuration.

    ```xml
    <Resource
      name="jdbc/commsRouterDB"
      auth="Container"
      type="javax.sql.DataSource"
      username="{USERNAME}"
      password="{PASSWORD}"
      driverClassName="com.mysql.jdbc.Driver"
      url="jdbc:mysql://{HOST}:{PORT}/{DATABASE_NAME}?zeroDateTimeBehavior=convertToNull&amp;useLegacyDatetimeCode=false&amp;useJDBCCompliantTimezoneShift=false&amp;serverTimezone=UTC&amp;useUnicode=yes&amp;characterEncoding=UTF-8"
      />
    ```
 
    Optionally it is possible to enable Database Connection Pool by setting additional options. See documentation [here][4] and [here][5].

    ```xml
    <Resource
      name="jdbc/commsRouterDB"
      auth="Container"
      type="javax.sql.DataSource"
      username="{USERNAME}"
      password="{PASSWORD}"
      driverClassName="com.mysql.jdbc.Driver"
      url="jdbc:mysql://{HOST}:{PORT}/{DATABASE_NAME}?zeroDateTimeBehavior=convertToNull&amp;useLegacyDatetimeCode=false&amp;useJDBCCompliantTimezoneShift=false&amp;serverTimezone=UTC&amp;useUnicode=yes&amp;characterEncoding=UTF-8"
      validationQuery="/* ping */"
      removeAbandonedOnBorrow="true"
      removeAbandonedOnMaintenance="true"
      removeAbandonedTimeout="60"
      logAbandoned="true"
    />
    ```

    For more help read the [documention][2].

    **Note:** Remember to update the following parameters above for your environment `{USERNAME}`, `{PASSWORD}`, `{HOST}`, `{PORT}` and `{DATABASE_NAME}`.

3. Add the Java Database Controller (JDBC) driver to the Java Virtual Machine (JVM). For Tomcat this means copying the [MySQL driver][6] `.jar` file to `CATALINA_BASE/lib`.
    
    ##### UNIX
    ```bash
    mv ~/Downloads/mysql-connector-java-5.1.XX/mysql-connector-java-5.1.XX-bin.jar CATALINA_BASE/lib
    ```

4. Comms Router uses Hibernate Java Persistence API (JPA). By default Hibernate creates tables in MySQL with the MyISAM engine which is non-transactional storage engine. Comms Router **requires a transactional storage engine** so a dialect is required to enable transactional storage at in the JVM running time.
    
    ##### UNIX
    Find or create `$CATALINA_BASE/bin/setenv.sh` then add dialect:
    ```bash
    export CATALINA_OPTS="$CATALINA_OPTS -Dhibernate.dialect=org.hibernate.dialect.MySQL57Dialect"
    ```
    Note that MySQL**5**Dialect is still using the MyISAM engine, so use MySQL**55**Dialect or MySQL**57**Dialect

    ##### Windows
    Find or create `%CATALINA_BASE%\bin\setenv.bat` then add dialect:
    ```bat
    set CATALINA_OPTS=%CATALINA_OPTS% -Dhibernate.dialect.storage_engine=innodb
    ```

5. Create a new `db-migrations/src/main/resources/liquibase.properties` file from the `db-migrations/src/main/resources/liquibase.properties.template` and make the following changes:
    
    ```properties
    verbose = true
    driver = com.mysql.jdbc.Driver
    changeLogFile = src/main/resources/db/changelog.yaml
    url = jdbc:mysql://localhost:3306/comms_router_core
    username = {USERNAME}
    password = {PASSWORD}
    ```

    **Note** Username and password should be the user that has all permissions granted to access and manage database e.g. `username=comms_router` and `password=comms_router_password`
    
 6. Populate and migrate the database to the latest version with:

    ```bash
    cd comms-router/db-migrations/
    mvn liquibase:update
    ``` 

    Also see [Manage DB Migrations] for help.


[1]: 
https://tomcat.apache.org/tomcat-8.0-doc/config/context.html  
"Apache Tomcat 8 Configuration Reference"

[2]: 
https://tomcat.apache.org/tomcat-8.0-doc/jndi-datasource-examples-howto.html 
"JNDI Datasource HOW-TO"

[3]:
https://dev.mysql.com/doc/connector-j/5.1/en/connector-j-reference-configuration-properties.html
"Driver/Datasource Class Names, URL Syntax and Configuration Properties for Connector/J"

[4]: 
https://tomcat.apache.org/tomcat-8.0-doc/jndi-datasource-examples-howto.html#Database_Connection_Pool_(DBCP_2)_Configurations 
"Database Connection Pool (DBCP 2) Configurations"

[5]: 
http://commons.apache.org/proper/commons-dbcp/configuration.html 
"BasicDataSource Configuration Parameters"

[6]: 
https://dev.mysql.com/downloads/connector/j/5.1.html 
"MySQL Connector/J"

[Manage DB Migrations]:
ManageDBMigrations.md
"Manage DB Migrations"
