Configuring Database Access
==

The application expects database access to be provided
from the Application Container via JNDI.

## Configure MySQL

### Create the Database

Create the database with `CHARACTER SET utf8 COLLATE utf8_general_ci`.

```sql
CREATE DATABASE `comms_router_core` CHARACTER SET `utf8` COLLATE `utf8_general_ci`;
```

### Create Users

1. You can, optionally, create two users - one for the migration management 
    (with create/drop table permissions) and another one for the application to manipulate the data.

    Migration user:
    
    ```mysql
    CREATE USER 'comms_migration'@'localhost' IDENTIFIED BY 'comms_migration_password';
    GRANT ALL ON `comms_router_core`.* TO 'comms_migration'@'localhost';
    ```
    
    Application user:
    
    ```mysql
    CREATE USER 'comms_router'@'localhost' IDENTIFIED BY 'comms_password';
    GRANT LOCK TABLES, SELECT, INSERT, DELETE, UPDATE ON `comms_router_core`.* TO 'comms_router'@'localhost';
    ```

2. Or just create one user for both with all privileges:

    ```mysql
    CREATE USER 'comms_router'@'localhost' IDENTIFIED BY 'comms_password';
    GRANT ALL ON `comms_router_core`.* TO 'comms_router'@'localhost';
    ```

## Configure Tomcat

Setting up the JNDI datasource on Tomcat.

1. You can create `comms-router-web.xml` in `$CATALINA_BASE/conf/[enginename]/[hostname]/`
    or use some other relevant file. [See Documentation][1]

    Alternatively this resource can be added to encompassing contexts defined
    in `$CATALINA_BASE/conf/context.xml` or in `$CATALINA_BASE/conf/[engine_name]/context.xml`.

2. In the chosen file you should put the datasource resource definition. [See Documentation][2]

    Example:

    ```xml
    <Resource
      name="jdbc/commsRouterDB"
      auth="Container"
      type="javax.sql.DataSource"
      username="{USERNAME}"
      password="{PASSWORD}"
      driverClassName="com.mysql.jdbc.Driver"
      url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}"/>
    ```

    The expected resource name is: `jdbc/commsRouterDB`

    The application requires [additional properties][3] to be set for MySQL:

    - `zeroDateTimeBehavior=convertToNull`  
    What should happen when the driver encounters DATETIME values that are composed 
    entirely of zeros (used by MySQL to represent invalid dates)?

    - `useLegacyDatetimeCode=false`  
    Use code for DATE/TIME/DATETIME/TIMESTAMP handling in result sets and statements that 
    consistently handles time zone conversions from client to server and back again, or use the 
    legacy code for these datatypes that has been in the driver for backwards-compatibility?    

    - `useJDBCCompliantTimezoneShift=false`  
    Should the driver use JDBC-compliant rules when converting TIME/TIMESTAMP/DATETIME values' 
    time zone information for those JDBC arguments which take a java.util.Calendar argument? 
    This is part of the legacy date-time code, thus the property has an effect 
    only when "useLegacyDatetimeCode=true."

    - `serverTimezone=UTC`  
    Override detection/mapping of time zone. 
    Used when time zone from server doesn't map to Java time zone

    - `useUnicode=true`  
    Should the driver use Unicode character encodings when handling strings?

    - `characterEncoding=UTF-8`  
    If 'useUnicode' is set to true, what character encoding should the driver use when dealing with strings?

    So the recommended URL for MySQL should looks like this:

    `url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}?zeroDateTimeBehavior=convertToNull&amp;useLegacyDatetimeCode=false&amp;useJDBCCompliantTimezoneShift=false&amp;serverTimezone=UTC&amp;useUnicode=yes&amp;characterEncoding=UTF-8"`

3. Optionally enable Database Connection Pool

    You can set additional options to enable Database Connection Pooling. 
    See documentation [here][4] and [here][5].

    Example:

    ```xml
    <Resource
      name="jdbc/commsRouterDB"
      auth="Container"
      type="javax.sql.DataSource"
      username="{USERNAME}"
      password="{PASSWORD}"
      driverClassName="com.mysql.jdbc.Driver"
      url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}"
      validationQuery="/* ping */"
      removeAbandonedOnBorrow="true"
      removeAbandonedOnMaintenance="true"
      removeAbandonedTimeout="60"
      logAbandoned="true"
    />
    ```


4. Provide the JDBC driver in the JVM path

    For Tomcat that means to copy the _.jar_ file in `$CATALINA_BASE/lib`.

    Ex. The [MySQL driver][6] is named _mysql-connector-java-5.1.XX-bin.jar_ and
    should be placed in the `lib` directory in the Tomcat installation.

5. Hibernate, the JPA provider we use, by default creates tables in MySQL with the MyISAM engine 
    which is non-transactional storage engine. 
    
    The CommsRouter **requires** _transactional_ storage engine. To enable that with MySQL 
    you should set one of those JVM options at start:
    - `-Dhibernate.dialect=org.hibernate.dialect.MySQL57Dialect` 
      - Note that MySQL**5**Dialect is still using the MyISAM engine, 
        so use MySQL**55**Dialect or MySQL**57**Dialect
    - `-Dhibernate.dialect.storage_engine=innodb`

    Ex. With Tomcat JVM properties are set like this:
    * UNIX: `$CATALINA_BASE/bin/setenv.sh`
    ```bash
    export CATALINA_OPTS="$CATALINA_OPTS -Dhibernate.dialect.storage_engine=innodb"
    ```
    * Windows: `%CATALINA_BASE%\bin\setenv.bat`
    ```bat
    set CATALINA_OPTS=%CATALINA_OPTS% -Dhibernate.dialect.storage_engine=innodb
    ```

6. Edit `db-migrations/src/main/resources/liquibase.properties` file and fill the details 
    for the user that has all permissions granted.  
    It should look like this:
    ```properties
    verbose = true
    driver = com.mysql.jdbc.Driver
    changeLogFile = src/main/resources/db/changelog.yaml
    url = jdbc:mysql://localhost:3306/comms_router_core
    username = {USERNAME}
    password = {PASSWORD}
    ```
    
    Then you can populate/migrate the database to the latest version with:
    ```bash
    cd db-migrations/
    mvn liquibase:update
    ``` 

    Also see [Manage DB Migrations]


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
