package com.softavail.commsrouter.util;

import static org.junit.Assert.assertEquals;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSet.Builder;
import java.util.Set;
import java.util.stream.IntStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;

/**
 * Created by @author mapuo on 20.09.17.
 */
public class IdGenTest {

  private static final Logger LOGGER = LogManager.getLogger(IdGenTest.class);

  @Test
  public void get()
      throws Exception {

    Builder<String> builder = ImmutableSet.builder();

    int count = 250_000;

    IntStream.range(0, count)
        .mapToObj(v -> Uuid.get())
        // .peek(LOGGER::debug)
        .forEach(builder::add);

    Set<String> set = builder.build();

    assertEquals("Set should have", count, set.size());
  }

}
