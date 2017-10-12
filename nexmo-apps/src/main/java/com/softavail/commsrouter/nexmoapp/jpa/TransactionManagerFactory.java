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
public class TransactionManagerFactory {

  private static final Logger LOGGER = LogManager.getLogger(TransactionManagerFactory.class);

  private static final String PERSISTENCE_UNIT_NAME = "com.softavail.comms-router.nexmoapps-pu";

  private final JpaTransactionManager transactionManager;
  private final ApplicationRepository applicationRepository;
  private final SessionRepository sessionRepository;
  private final ModuleRepository expressionRepository;
  private final EntityMappers entityMappers;
  private final SessionReferenceRepository sessionReferenceRepository;

  public TransactionManagerFactory() {
    EntityManagerFactory emf = Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME);
    transactionManager = new JpaTransactionManager(emf);
    applicationRepository = new ApplicationRepository(transactionManager);
    sessionRepository = new SessionRepository(transactionManager);
    expressionRepository = new ModuleRepository(transactionManager);
    sessionReferenceRepository = new SessionReferenceRepository(transactionManager);
    entityMappers = new EntityMappers();
  }

  public void close() {
    transactionManager.close();
  }

  public JpaTransactionManager getTransactionManager() {
    return transactionManager;
  }

  public ApplicationRepository getApplicationRepository() {
    return applicationRepository;
  }

  public SessionRepository getSessionRepository() {
    return sessionRepository;
  }

  public SessionReferenceRepository getSessionReferenceRepository() {
    return sessionReferenceRepository;
  }

  public ModuleRepository getExpressionRepository() {
    return expressionRepository;
  }

  public EntityMappers getEntityMappers() {
    return entityMappers;
  }

}
