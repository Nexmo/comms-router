
package com.softavail.commsrouter.domain.test;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.jpa.test.TestBase;

import org.junit.Test;
import java.util.List;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;

/**
 * @author G.Ivanov
 */
public class RouterTest extends TestBase {

  @Test
  public void testGetObjectById_success() throws NotFoundException {
    Router router = app.db.router.getByRef(em, "01");
    assertNotNull(router);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testGetAll_success() throws CommsRouterException {
    List<Router> routers = app.db.router.list(em);
    assertEquals(2, routers.size());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPersist_success() throws CommsRouterException {
    createRouter("name_two", "description_two", "03");
    List<Router> routers = app.db.router.list(em);
    assertNotNull(routers);
    assertEquals(3, routers.size());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDelete_success() throws CommsRouterException {
    app.db.router.deleteByRef(em, "01");
    List<Router> routers = app.db.router.list(em);
    assertEquals(2, routers.size());
  }

}
