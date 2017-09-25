/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.domain.Plan;

/**
 * @author ikrustev
 */
public class PlanRepository extends RouterObjectRepository<Plan> {

  public PlanRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

}
