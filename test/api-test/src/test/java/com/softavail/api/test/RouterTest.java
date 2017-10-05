package com.softavail.api.test;
import static io.restassured.RestAssured.*;
import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;
import org.junit.jupiter.api.Test;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import java.util.HashMap;
import java.net.URL;
import java.net.MalformedURLException;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.AgentDto;
/**
 * Unit test for simple App.
 */
public class RouterTest

{

    @Test
    public void createRouter() {
        // best case
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObjectId id = r.create(routerArg);
        RouterDto router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));
        r.delete();
    }
    @Test
    public void newRouterWithSpecifiedId() {
        // create router using specified id
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        String routerId = "newRouterId";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        state.put(CommsRouterResource.ROUTER,routerId);
        ApiObjectId id = r.replace(routerArg);

        RouterDto router = r.get();
        assertThat(router.getName(),is(name));
        assertThat(router.getDescription(),is(description));
        assertThat(router.getId(),is(routerId));

        r.delete();
    }
    @Test
    public void replaceRouter() {
        // replace existing router
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObjectId id = r.create(routerArg);
        RouterDto router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));

        ApiObjectId id1 = r.replace( new CreateRouterArg());// replace with null values
        assertThat(id.getId(),is(id1.getId()));
        router = r.get();
        assertThat(router.getName(), nullValue());
        assertThat(router.getDescription(), nullValue());

        r.delete();
    }

    @Test
    public void replaceRouterRouterResourcesAreThere() {
        // replace existing router and check that Queue is here after the replacement
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObjectId id = r.create(routerArg);
        RouterDto router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));

        Queue q = new Queue(state);
        CreateQueueArg qArg = new CreateQueueArg();
        qArg.setPredicate("1==1");
        ApiObjectId qid = q.create(new CreateQueueArg());

        ApiObjectId id1 = r.replace( new CreateRouterArg());// replace with null values
        // check that queue is still there
        QueueDto queue = q.get();
        assertThat(queue.getDescription(),nullValue());
        assertThat(q.list(),hasItems(hasProperty("id", is(qid.getId()))));


        assertThat(id.getId(),is(id1.getId()));
        router = r.get();
        assertThat(router.getName(), nullValue());
        assertThat(router.getDescription(), nullValue());
        q.delete();
        r.delete();
    }
    @Test
    public void updateRouterFields() {
        // update values of the existing router
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObjectId id = r.create(routerArg);
        RouterDto router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));

        r.update( new CreateRouterArg());// should not change values
        router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));

        routerArg.setDescription("changed");
        routerArg.setName(null);
        r.update( routerArg);// should change only description
        router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is("changed"));

        routerArg.setDescription(null);
        routerArg.setName("changedName");
        r.update( routerArg);// should change only description
        router = r.get();
        assertThat(router.getName(), is("changedName"));
        assertThat(router.getDescription(), is("changed"));

        r.delete();
    }

    @Test
    public void doubleDeleteRouter() {
        // replace existing router
        HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
        Router r = new Router(state);
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObjectId id = r.create(routerArg);
        RouterDto router = r.get();
        assertThat(router.getName(), is(name));
        assertThat(router.getDescription(), is(description));
        r.delete();
        r.delete();
    }
}
