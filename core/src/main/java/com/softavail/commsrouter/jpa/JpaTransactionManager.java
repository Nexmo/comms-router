/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.exception.CommsRouterException;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.OptimisticLockException;
import javax.persistence.PersistenceException;

/**
 *
 * @author ikrustev
 */
public class JpaTransactionManager {

  private final EntityManagerFactory emf;

  public JpaTransactionManager(EntityManagerFactory emf) {
    this.emf = emf;
  }

  public <RESULT> RESULT execute(int lockRetryCount, TransactionLogic<RESULT> transactionLogic)
      throws CommsRouterException {

    EntityManager em = emf.createEntityManager();
    try {
      EntityTransaction dbTransaction = em.getTransaction();
      for (;;) {
        try {
          dbTransaction.begin();
          RESULT result = transactionLogic.run(em);
          dbTransaction.commit();
          return result;
        } catch (PersistenceException ex) {
          if (dbTransaction.isActive()) {
            dbTransaction.rollback();
          }
          Throwable cause = ex.getCause();
          if (cause != null && cause instanceof OptimisticLockException && lockRetryCount > 0) {
            --lockRetryCount;
            continue;
          }
          throw ex;
        } catch (Exception ex) {
          if (dbTransaction.isActive()) {
            dbTransaction.rollback();
          }
          throw ex;
        }
      }
    } finally {
      em.close();
    }
  }

  public <RESULT> RESULT execute(TransactionLogic<RESULT> transactionLogic)
      throws CommsRouterException {
    return execute(0, transactionLogic);
  }

  public <RESULT> RESULT executeWithLockRetry(TransactionLogic<RESULT> transactionLogic)
      throws CommsRouterException {
    // TODO: default retry count config
    return execute(10, transactionLogic);
  }

  public void executeVoid(VoidTransactionLogic voidTransactionLogic) throws CommsRouterException {
    execute(voidTransactionLogic);
  }

  public void close() {
    emf.close();
  }

}
