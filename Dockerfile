FROM openjdk:8-jdk as build

RUN wget http://archive.apache.org/dist/maven/maven-3/3.5.4/binaries/apache-maven-3.5.4-bin.tar.gz \
    && tar -zxvf apache-maven-3.5.4-bin.tar.gz \
    && rm apache-maven-3.5.4-bin.tar.gz \
    && mv apache-maven-3.5.4 /usr/lib/mvn

ENV PATH=$PATH:/usr/lib/mvn/bin

WORKDIR /usr/src/
COPY . .
RUN mvn install

FROM tomcat:8.5.33-jre8-alpine

RUN apk add --update ttf-dejavu && rm -f /var/cache/apk/*

RUN wget http://archive.apache.org/dist/maven/maven-3/3.5.4/binaries/apache-maven-3.5.4-bin.tar.gz \
    && tar -zxvf apache-maven-3.5.4-bin.tar.gz \
    && rm apache-maven-3.5.4-bin.tar.gz \
    && mv apache-maven-3.5.4 /usr/lib/mvn

ENV PATH=$PATH:/usr/lib/mvn/bin

RUN wget https://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-8.0.12.tar.gz \
    && tar -zxvf mysql-connector-java-8.0.12.tar.gz \
    && rm mysql-connector-java-8.0.12.tar.gz \
    && mv mysql-connector-java-8.0.12/mysql-connector-java-8.0.12.jar /usr/local/tomcat/lib

RUN rm -rf /usr/local/tomcat/webapps/ROOT \
    && rm -rf /usr/local/tomcat/webapps/examples \
    && rm -rf /usr/local/tomcat/webapps/host-manager \
    && rm -rf /usr/local/tomcat/webapps/manager

COPY ./db-migrations /usr/local/tomcat/db-migrations
COPY ./pom.xml /usr/local/tomcat/pom.xml
COPY --from=build /usr/src/web/target/comms-router-web.war /usr/local/tomcat/webapps/ROOT.war
