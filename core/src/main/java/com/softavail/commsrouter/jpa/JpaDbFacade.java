/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.app.CoreConfiguration;
import com.softavail.commsrouter.domain.Agent;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Task;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

/**
 * @author ikrustev
 */
public class JpaDbFacade {

  private static final String PERSISTENCE_UNIT = "com.softavail.comms-router.core-pu";

  public final JpaTransactionManager transactionManager;

  public final GenericRepository<Router> router;
  public final QueueRepository queue;
  public final RouterObjectRepository<Plan> plan;
  public final RouterObjectRepository<Agent> agent;
  public final RouterObjectRepository<Task> task;

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

  }

  public void close() {
    transactionManager.close();
  }

}
