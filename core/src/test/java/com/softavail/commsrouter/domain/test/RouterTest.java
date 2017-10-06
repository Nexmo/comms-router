
package com.softavail.commsrouter.domain.test;

import com.softavail.commsrouter.domain.Router;

import org.junit.Test;
import java.util.List;
import javax.persistence.Query;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;

/**
 * @author G.Ivanov
 */
public class RouterTest extends TestBase {

  @Test
  public void testGetObjectById_success() {
    Router router = em.find(Router.class, "01");
    assertNotNull(router);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testGetAll_success() {
    Query query = em.createQuery("SELECT e FROM Router e");
    List<Router> routers = query.getResultList();
    assertEquals(1, routers.size());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPersist_success() {
    createRouter("name_two", "description_two", "02");
    Query query = em.createQuery("SELECT e FROM Router e");
    List<Router> routers = query.getResultList();
    assertNotNull(routers);
    assertEquals(2, routers.size());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDelete_success() {
    Router router = em.find(Router.class, "01");

    em.getTransaction().begin();
    em.remove(router);
    em.getTransaction().commit();

    Query query = em.createQuery("SELECT e FROM Router e");
    List<Router> routers = query.getResultList();

    assertEquals(0, routers.size());
  }

}
