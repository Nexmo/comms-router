/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.google.common.base.Throwables;

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ReferenceIntegrityViolationException;

import java.util.Optional;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.OptimisticLockException;
import javax.persistence.PersistenceException;

/**
 * @author ikrustev
 */
public class JpaTransactionManager {

  private final EntityManagerFactory emf;
  private final int lockRetryCount;

  public JpaTransactionManager(EntityManagerFactory emf, int lockRetryCount) {
    this.emf = emf;
    this.lockRetryCount = lockRetryCount;
  }

  public <RESULT> RESULT execute(int lockRetryCount, TransactionLogic<RESULT> transactionLogic)
      throws CommsRouterException {

    EntityManager em = emf.createEntityManager();
    try {
      EntityTransaction dbTransaction = em.getTransaction();
      for (; ; ) {
        try {
          dbTransaction.begin();
          RESULT result = transactionLogic.run(em);
          dbTransaction.commit();
          return result;
        } catch (PersistenceException ex) {
          if (dbTransaction.isActive()) {
            dbTransaction.rollback();
          }

          if (OptimisticLockException.class.isInstance(ex.getCause()) && lockRetryCount > 0) {
            --lockRetryCount;
            continue;
          }

          // Hibernate does not follow JPA 2 specs and wraps ConstraintViolation in RollbackEx
          Class<javax.validation.ConstraintViolationException> javaxConstraint =
              javax.validation.ConstraintViolationException.class;
          if (javaxConstraint.isInstance(ex.getCause())) {
            throw javaxConstraint.cast(ex.getCause());
          }

          // Find Integrity Constraint Violations like foreign key constraint
          Class<org.hibernate.exception.ConstraintViolationException> hibernateConstraint =
              org.hibernate.exception.ConstraintViolationException.class;
          Optional<Throwable> throwable = Throwables.getCausalChain(ex).stream()
              .filter((hibernateConstraint)::isInstance)
              .findFirst()
              .map(Throwable::getCause);
          if (throwable.isPresent()) {
            throw new ReferenceIntegrityViolationException(throwable.get());
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

    return execute(lockRetryCount, transactionLogic);
  }

  public void executeVoid(VoidTransactionLogic voidTransactionLogic)
      throws CommsRouterException {

    execute(voidTransactionLogic);
  }

  public void close() {
    emf.close();
  }

}
