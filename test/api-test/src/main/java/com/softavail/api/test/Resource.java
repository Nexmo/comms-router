package com.softavail.api.test;
import java.util.HashMap;
import io.restassured.RestAssured;


public class Resource
{
  private HashMap<CommsRouterResource,String> state;
  public Resource(HashMap<CommsRouterResource,String> state){
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    if (System.getProperty("autHost")==null){
      RestAssured.baseURI = "http://localhost:8080";
    } else {
      RestAssured.baseURI = System.getProperty("autHost"); // you can specify it using -DautHost=http://localhost:8080
    }

    RestAssured.basePath= "/comms-router-web/api";
    this.state=state;
  }
  public HashMap<CommsRouterResource,String> state(){
    return this.state;
  }

}
