/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.domain.Router;

/**
 * @author ikrustev
 */
public class RouterRepository extends GenericRepository<Router> {

  public RouterRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
