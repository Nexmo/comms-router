package com.softavail.commsrouter.api.dto.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Rule;
import com.softavail.commsrouter.domain.Task;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.junit.Before;
import org.junit.Test;

/**
 * Created by @author mapuo on 21.09.17.
 */
public class RouterObjectTest {

  private static final Logger LOGGER = LogManager.getLogger(RouterObjectTest.class);

  private Plan planP1R1;
  private Plan planP1R2;
  private Plan planP1R1new;
  private Task task;

  @Before
  public void setUp() throws Exception {
    planP1R1 = new Plan();
    planP1R1.setId("P1");
    planP1R1.setRouterId("R1");

    planP1R1new = new Plan();
    planP1R1new.setId("P1");
    planP1R1new.setRouterId("R1");

    planP1R2 = new Plan();
    planP1R2.setId("P1");
    planP1R2.setRouterId("R2");

    task = new Task();
    task.setId("P1");
    task.setRouterId("R1");
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
