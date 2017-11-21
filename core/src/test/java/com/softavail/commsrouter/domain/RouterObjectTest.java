package com.softavail.commsrouter.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.junit.Before;
import org.junit.Test;

/**
 * Created by @author mapuo on 21.09.17.
 */
public class RouterObjectTest {

  private static final Logger LOGGER = LogManager.getLogger(RouterObjectTest.class);

  private Router routerR1;
  private Router routerR2;
  private Plan planP1R1;
  private Plan planP1R2;
  private Plan planP1R1new;
  private Task task;

  @Before
  public void setUp() throws Exception {

    routerR1 = new Router();
    routerR1.setRef("R1");

    routerR2 = new Router();
    routerR2.setRef("R2");

    planP1R1 = new Plan();
    planP1R1.setRef("P1");
    planP1R1.setRouter(routerR1);

    planP1R1new = new Plan();
    planP1R1new.setRef("P1");
    planP1R1new.setRouter(routerR1);

    planP1R2 = new Plan();
    planP1R2.setRef("P1");
    planP1R2.setRouter(routerR2);

    task = new Task();
    task.setRef("P1");
    task.setRouter(routerR2);
  }

  @Test
  public void equalsTest() throws Exception {
    assertTrue("The same object",
        planP1R1.equals(planP1R1));

    assertTrue("Diff object",
        planP1R1.equals(planP1R1new));

    assertFalse("Should not equals",
        planP1R1.equals(planP1R2));

    assertFalse("Diff class",
        planP1R1.equals(task));

    assertFalse("Diff class",
        planP1R1.equals(new Rule()));

  }

  @Test
  public void hashCodeTest() throws Exception {
    assertEquals("The same object",
        planP1R1.hashCode(), planP1R1.hashCode());

    assertEquals("Diff object",
        planP1R1.hashCode(), planP1R1new.hashCode());

    assertNotEquals("Should not equals",
        planP1R1.hashCode(), planP1R2.hashCode());

    assertNotEquals("Diff class",
        planP1R1.hashCode(), task.hashCode());
  }

}
