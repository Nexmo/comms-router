package com.softavail.commsrouter.domain.result;

import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Task;

/**
 * Created by @author mapuo on 18.10.17.
 */
public class MatchResult {

  public Task task;
  public Agent agent;

  public MatchResult(Task task, Agent agent) {
    this.task = task;
    this.agent = agent;
  }

}
