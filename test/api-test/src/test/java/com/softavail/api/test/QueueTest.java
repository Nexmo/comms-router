package com.softavail.api.test;
import static io.restassured.RestAssured.*;
import static io.restassured.matcher.RestAssuredMatchers.*;
import static org.hamcrest.Matchers.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.beans.HasPropertyWithValue.hasProperty;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.DisplayName;
import com.softavail.commsrouter.api.dto.arg.CreateRouterArg;
import com.softavail.commsrouter.api.dto.arg.CreateQueueArg;
import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.arg.CreateAgentArg;
import java.util.HashMap;
import java.net.URL;
import java.net.MalformedURLException;
import com.softavail.commsrouter.api.dto.model.ApiObject;
import com.softavail.commsrouter.api.dto.model.RouterDto;
import com.softavail.commsrouter.api.dto.model.QueueDto;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.TaskDto;
import com.softavail.commsrouter.api.dto.model.AgentDto;
/**
 * Unit test for simple App.
 */
@TestInstance(Lifecycle.PER_CLASS)

@DisplayName("Queue Test")
public class QueueTest
{
    private HashMap<CommsRouterResource,String> state = new HashMap<CommsRouterResource,String>();
    private Router r = new Router(state);
    @BeforeAll
    public void createRouter() {
        // best case
        String description = "Router description";
        String name = "router-name";
        CreateRouterArg routerArg= new CreateRouterArg();
        routerArg.setDescription(description);
        routerArg.setName(name);
        ApiObject id = r.create(routerArg);
    }
    @AfterAll
    public void deleteRouter() {
        r.delete();
    }
    @Test
    @DisplayName("Create new queue.")
    public void createQueue() {
        // best case
        String description="queue description";
        String predicate="1==1";
        CreateQueueArg queueArg= new CreateQueueArg();
        queueArg.setDescription(description);
        queueArg.setPredicate(predicate);
        Queue q = new Queue(state);
        ApiObject id = q.create(queueArg);
        QueueDto queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(description));
        q.delete();
    }
    @Test
    @DisplayName("Create queue with specified id")
    public void createQueueWithSpecifiedId() {
        // put request to not existing queue
        String description="queue description";
        String predicate="1==1";
        String queueId="Queue-id";
        CreateQueueArg queueArg= new CreateQueueArg();
        queueArg.setDescription(description);
        queueArg.setPredicate(predicate);
        Queue q = new Queue(state);
        state.put(CommsRouterResource.QUEUE,queueId);
        ApiObject id = q.replace(queueArg);
        QueueDto queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(description));
        assertThat(queue.getId(), is(queueId));

        q.delete();
    }
    @Test
    @DisplayName("Replace existing queue")
    public void replaceExistingQueue() {
        // put request to not existing queue
        String description="queue description";
        String predicate="1==1";
        String queueId="Queue-id";
        CreateQueueArg queueArg= new CreateQueueArg();
        queueArg.setDescription(description);
        queueArg.setPredicate(predicate);
        Queue q = new Queue(state);
        state.put(CommsRouterResource.QUEUE,queueId);
        ApiObject id = q.replace(queueArg);
        QueueDto queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(description));
        assertThat(queue.getId(), is(queueId));

        queueArg.setDescription(null);
        queueArg.setPredicate(null);

        id = q.replace(queueArg);
        queue = q.get();
        assertThat(queue.getPredicate(), is(nullValue()));
        assertThat(queue.getDescription(), is(nullValue()));
        assertThat(queue.getId(), is(queueId));

        q.delete();
    }
    @Test
    @DisplayName("Set parameters")
    void updateParameters(){
        String description="queue description";
        String predicate="1==1";
        CreateQueueArg queueArg= new CreateQueueArg();
        queueArg.setDescription(description);
        queueArg.setPredicate(predicate);
        Queue q = new Queue(state);
        ApiObject id = q.create(queueArg);
        QueueDto queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(description));

        queueArg.setDescription(null);
        queueArg.setPredicate(null);

        q.update(queueArg);
        queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(description));
        String newDescription="queue-new-description";
        queueArg.setDescription(newDescription);
        queueArg.setPredicate(null);

        q.update(queueArg);
        queue = q.get();
        assertThat(queue.getPredicate(), is(predicate));
        assertThat(queue.getDescription(), is(newDescription));
        String newPredicate="2==2";
        queueArg.setDescription(null);
        queueArg.setPredicate(newPredicate);

        q.update(queueArg);
        queue = q.get();
        assertThat(queue.getPredicate(), is(newPredicate));
        assertThat(queue.getDescription(), is(newDescription));

        q.delete();
    }
    @Test
    @DisplayName("empty queue")
    void emptyQueue(){
        String description="queue description";
        String predicate="1==1";
        CreateQueueArg queueArg= new CreateQueueArg();
        queueArg.setDescription(description);
        queueArg.setPredicate(predicate);
        Queue q = new Queue(state);
        ApiObject id = q.create(queueArg);
        assertThat(q.size(), is(0));
        q.delete();
    }
}
