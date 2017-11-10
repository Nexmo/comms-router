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

package com.softavail.commsrouter.jpa.result;

import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.jpa.JpaTransactionManager;

import javax.persistence.EntityManager;
import javax.persistence.Query;

public class TaskEnumerableResult extends EnumeratableResult<Task> {

  private TaskState stateFiler = null;
  
  public TaskEnumerableResult(JpaTransactionManager transactionManager) {
    super(transactionManager, 1000);
    this.stateFiler = null;
  }

  public TaskEnumerableResult(JpaTransactionManager transactionManager, TaskState filterByState) {
    super(transactionManager, 1000);
    this.stateFiler = filterByState;
  }

  @Override
  protected Query createQuery(EntityManager em) {
    
    StringBuilder qlBuilder = new StringBuilder(); 
    qlBuilder.append("SELECT t FROM Task t");
    if (this.stateFiler != null) {
      qlBuilder.append(" WHERE t.state = :taskState");
    }
    
    qlBuilder.append(" ORDER BY create_date ASC");
    
    Query query = em.createQuery(qlBuilder.toString());
    if (this.stateFiler != null) {
      query.setParameter("taskState", this.stateFiler);
    }
    
    return query;
  }

  public TaskState getStateFiler() {
    return stateFiler;
  }
}
