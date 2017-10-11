package com.softavail.commsrouter.nexmoapp.jpa;

import com.softavail.commsrouter.jpa.JpaTransactionManager;
import com.softavail.commsrouter.nexmoapp.domain.dto.mappers.EntityMappers;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

/**
 * Created by @author mapuo on 10.10.17.
 */
@WebListener("Provide EntityManager")
public class TransactionManagerFactory implements ServletContextListener {

  private static final Logger LOGGER = LogManager.getLogger(TransactionManagerFactory.class);

  private static final String PERSISTENCE_UNIT_NAME = "com.softavail.comms-router.nexmoapps-pu";

  private static JpaTransactionManager transactionManager;
  private static ApplicationRepository applicationRepository;
  private static SessionRepository sessionRepository;
  private static ModuleRepository expressionRepository;
  private static EntityMappers entityMappers;

  @Override
  public void contextInitialized(ServletContextEvent sce) {
    LOGGER.debug("contextInitialized: {}", sce);

    EntityManagerFactory emf = Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME);
    transactionManager = new JpaTransactionManager(emf);
    applicationRepository = new ApplicationRepository(transactionManager);
    sessionRepository = new SessionRepository(transactionManager);
    expressionRepository = new ModuleRepository(transactionManager);
    entityMappers = new EntityMappers();
  }

  @Override
  public void contextDestroyed(ServletContextEvent sce) {
    LOGGER.debug("contextDestroyed: {}", sce);
    transactionManager.close();
  }

  public static JpaTransactionManager getTransactionManager() {
    return transactionManager;
  }

  public static ApplicationRepository getApplicationRepository() {
    return applicationRepository;
  }

  public static SessionRepository getSessionRepository() {
    return sessionRepository;
  }

  public static ModuleRepository getExpressionRepository() {
    return expressionRepository;
  }

  public static EntityMappers getEntityMappers() {
    return entityMappers;
  }

}
