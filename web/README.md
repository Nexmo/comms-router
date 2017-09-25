Comms Router Web API
==

Configuring Database Access
--

The application expects database access to be provided
from the Application Container via JNDI.

### Tomcat

Setting up the JNDI datasource on Tomcat.

1. You can create `comms-router-web-api.xml` in `$CATALINA_BASE/conf/[enginename]/[hostname]/`
    or use some other relevant file. [See Documentation][1]

    Alternatively this resource can be added to encompassing contexts defined in `$CATALINA_BASE/conf/context.xml` or in `$CATALINA_BASE/conf/[engine_name]/context.xml`.

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

    The application requires additional properties to be set for MySQL:

    - `zeroDateTimeBehavior=convertToNull`
    - `useJDBCCompliantTimezoneShift=false`
    - `useLegacyDatetimeCode=false`
    - `serverTimezone=UTC`
    - and in case you don't have SSL connection: `useSSL=false`

    So the recommended url should looks like:

    `url="jdbc:mysql://{HOST}:{PORT}/{DB_NAME}?zeroDateTimeBehavior=convertToNull&amp;useJDBCCompliantTimezoneShift=false&amp;useLegacyDatetimeCode=false&amp;serverTimezone=UTC&amp;useSSL=false"`

3. Provide the JDBC driver in the JVM path

    For Tomcat that means to copy the _.jar_ file in `$CATALINA_BASE/lib`.

    Ex. The [MySql driver][3] is named _mysql-connector-java-5.1.XX-bin.jar_ and
    should be placed in the `lib` directory in the Tomcat installation.


[1]: https://tomcat.apache.org/tomcat-8.0-doc/config/context.html  "Apache Tomcat 8 Configuration Reference"
[2]: https://tomcat.apache.org/tomcat-8.0-doc/jndi-datasource-examples-howto.html "JNDI Datasource HOW-TO"
[3]: https://dev.mysql.com/downloads/connector/j/5.1.html "MySQL Connector/J"
