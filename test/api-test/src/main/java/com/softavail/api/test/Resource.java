package com.softavail.api.test;
import java.util.HashMap;
public class Resource
{
    private HashMap<CommsRouterResource,String> state;
    public Resource(HashMap<CommsRouterResource,String> state){
        this.state=state;
    }
    public HashMap<CommsRouterResource,String> state(){
        return this.state;
    }
}
