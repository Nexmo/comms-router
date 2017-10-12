NexmoApps
==

NexmoApps provides integration with the Comms Router and the Nexmo Voice Applications.

## Create user and DB in MySQL

The application needs it own database. 
To create one you will need to login to your MySQL instance and issue the following commands:
1. `CREATE DATABASE 'nexmo-apps';`
2. `CREATE USER 'nexmo-apps'@'localhost' IDENTIFIED BY 'nexmo-apps-pass';`
3. `GRANT ALL ON 'nexmo_apps'.* TO 'nexmo-apps'@'localhost';`

_* These are example values, please, change them as you see fit._

## Set JNDI Data Source

Configure JNDI data source with name "_jdbc/nexmoAppsDB_" in one of the following places:

  * `$CATALINA_BASE/conf/[enginename]/[hostname]/nexmo-apps.xml` 
  * `$CATALINA_BASE/conf/context.xml` 
  * `$CATALINA_BASE/conf/[engine_name]/context.xml`.

Example JNDI declaration:

```xml
<Resource
  name="jdbc/nexmoAppsDB"
  auth="Container"
  type="javax.sql.DataSource"
  username="{USERNAME}"
  password="{PASSWORD}"
  driverClassName="com.mysql.jdbc.Driver"
  url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}"/>
```

The application requires additional properties to be set for MySQL:

  - `zeroDateTimeBehavior=convertToNull`
  - `useJDBCCompliantTimezoneShift=false`
  - `useLegacyDatetimeCode=false`
  - `serverTimezone=UTC`
  - and in case you don't have SSL connection: `useSSL=false`

So the recommended url should looks like:

  `url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}?zeroDateTimeBehavior=convertToNull&amp;
  useJDBCCompliantTimezoneShift=false&amp;useLegacyDatetimeCode=false&amp;serverTimezone=UTC&amp;useSSL=false"`

## Configuration

The application requires settings file to be provided with the following keys:

- `app.commsRouterUrl` - The URL of the Comms Router API
- `app.callbackBaseUrl` - The URL of the application where Comms Router will send callbacks 

#### JVM Parameter

Add a system property of the JVM with key `nexmo.apps.config.file` and value - the full path to the file.

Ex. `java -Dnexmo.apps.config.file=/configDir/application.properties`.
Ex. Tomcat has `bin/setenv.sh` where you can say
```bash
export CATALINA_OPTS="$CATALINA_OPTS -Dnexmo.apps.config.file=/configDir/application.properties"
```

#### Servlet Context

Alternatively you can set the same key as a context parameter. [See Documentation][1]

In `$CATALINA_BASE/conf/[enginename]/[hostname]/nexmo-apps.xml` you can add the following:

```xml
<Parameter
    name="nexmo.apps.config.file"
    value="/configDir/application.properties" />
```

[1]: https://tomcat.apache.org/tomcat-8.0-doc/config/context.html#Context_Parameters "The Context Container > Context Parameters"
