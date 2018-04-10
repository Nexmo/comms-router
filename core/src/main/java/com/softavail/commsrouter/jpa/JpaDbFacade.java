/*
 * Copyright 2017 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.app.CoreConfiguration;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Skill;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

/**
 * @author ikrustev
 */
public class JpaDbFacade {

  private static final String PERSISTENCE_UNIT = "com.softavail.comms-router.core-pu";

  public final JpaTransactionManager transactionManager;

  public final RouterRepository router;
  public final QueueRepository queue;
  public final PlanRepository plan;
  public final RouterObjectRepository<Agent> agent;
  public final TaskRepository task;
  public final RouterObjectRepository<Skill> skill;

  public JpaDbFacade() {
    this(CoreConfiguration.DEFAULT, PERSISTENCE_UNIT);
  }

  public JpaDbFacade(CoreConfiguration configuration) {
    this(configuration, PERSISTENCE_UNIT);
  }

  public JpaDbFacade(String unit) {
    this(CoreConfiguration.DEFAULT, unit);
  }

  public JpaDbFacade(CoreConfiguration configuration, String unit) {

    EntityManagerFactory emf = Persistence.createEntityManagerFactory(unit);

    transactionManager = new JpaTransactionManager(emf, configuration.getJpaLockRetryCount());

    this.router = new RouterRepository(transactionManager);
    this.queue = new QueueRepository(transactionManager);
    this.plan = new PlanRepository(transactionManager);
    this.agent = new AgentRepository(transactionManager);
    this.task = new TaskRepository(transactionManager);
    this.skill = new SkillRepository(transactionManager);

  }

  public void close() {
    transactionManager.close();
  }

}
