package com.softavail.api.test;
import java.util.HashMap;
import io.restassured.RestAssured;


public class Resource
{
  private HashMap<CommsRouterResource,String> state;
  public Resource(HashMap<CommsRouterResource,String> state){
    RestAssured.enableLoggingOfRequestAndResponseIfValidationFails();
    RestAssured.baseURI = System.getProperty("autHost");
    RestAssured.basePath= "/comms-router-web/api";
    this.state=state;
  }
  public HashMap<CommsRouterResource,String> state(){
    return this.state;
  }

}
