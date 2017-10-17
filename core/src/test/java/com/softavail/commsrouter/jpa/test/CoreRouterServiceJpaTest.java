/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.jpa.test;

import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.UpdateRouterArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * @author G.Ivanov
 */
public class CoreRouterServiceJpaTest extends TestBase {

    //Testing the get method inherited from CoreApiObjectService
    @Test
    public void getTest() throws CommsRouterException {
        RouterDto router = routerService.get("01");
        assertEquals("name_one",router.getName());
        assertEquals("description_one",router.getDescription());
    }

    //Testing the list method inherited from CoreApiObjectService
    @Test
    public void listTest() throws CommsRouterException {
        List<RouterDto> routers = routerService.list();
        assertEquals(2,routers.size());
    }

    //Testing the delete method inherited from CoreApiObjectService
    @Test
    public void deleteTest() throws CommsRouterException {
        List<RouterDto> routers = routerService.list();
        assertEquals(2,routers.size());
        routerService.delete("01");
        routers = routerService.list();
        assertEquals(1,routers.size());
    }

    //Testing the create method
    @Test
    public void createTest() throws CommsRouterException {
        CreateRouterArg createArg = returnNewCreateRouterArg("name_three","description_three");
        RouterDto newRouter = routerService.create(createArg);
        //Assert that the router was created
        assertEquals("name_three", newRouter.getName());
        assertEquals("description_three", newRouter.getDescription());
        //Get the new router by ID
        RouterDto router = routerService.get(newRouter.getId());
        assertEquals("name_three",router.getName());
        assertEquals("description_three",router.getDescription());
    }

    //Testing the update method
    @Test
    public void updateTest() throws CommsRouterException {
        UpdateRouterArg updateArg = returnNewUpdateRouterArg("name_nine","description_nine");
        ApiObjectId id = new ApiObjectId("02");
        routerService.update(updateArg, id);
        //Get updated router by ID
        RouterDto router = routerService.get("02");
        assertEquals("name_nine",router.getName());
        assertEquals("description_nine",router.getDescription());
    }

    //Testing the put method
    @Test
    public void putTest() throws CommsRouterException {
        CreateRouterArg createArg = returnNewCreateRouterArg("name_ten","description_ten");
        ApiObjectId id = new ApiObjectId("01");
        RouterDto newRouter = routerService.put(createArg, id);
        assertEquals("description_ten", newRouter.getDescription());
        assertEquals("name_ten", newRouter.getName());
        //Get new router by ID
        RouterDto router = routerService.get("01");
        assertEquals("name_ten",router.getName());
        assertEquals("description_ten",router.getDescription());
    }

    @Test
    public void getDtoEntityTest() {
        Class<RouterDto> newRouter = routerService.getDtoEntityClass();
    }

}
