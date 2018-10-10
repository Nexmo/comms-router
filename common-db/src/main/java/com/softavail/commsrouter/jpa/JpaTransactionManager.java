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
              .findFirst();

          if (throwable.isPresent()) {
            org.hibernate.exception.ConstraintViolationException hibernateException =
                (org.hibernate.exception.ConstraintViolationException) throwable.get();
            ReferenceIntegrityViolationException newEx =
                new ReferenceIntegrityViolationException(hibernateException);
            newEx.setConstraintName(hibernateException.getConstraintName());
            throw newEx;
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

  public void executeVoidWithLockRetry(VoidTransactionLogic voidTransactionLogic)
      throws CommsRouterException {

    execute(lockRetryCount, voidTransactionLogic);
  }

  public void close() {
    emf.close();
  }

}
