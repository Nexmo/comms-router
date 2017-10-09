/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.exception.CommsRouterException;

import javax.persistence.EntityManager;

/**
 *
 * @author ikrustev
 */
public interface VoidTransactionLogic extends TransactionLogic<Void> {

  abstract void voidRun(EntityManager em) throws CommsRouterException;

  @Override
  default Void run(EntityManager em) throws CommsRouterException {
    voidRun(em);
    return null;
  }

}
