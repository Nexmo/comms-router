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
