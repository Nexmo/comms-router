/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.jpa;

import com.softavail.commsrouter.api.dto.model.TaskState;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.domain.Task;
import com.softavail.commsrouter.jpa.result.TaskEnumerableResult;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class TaskRepository extends RouterObjectRepository<Task> {

  public TaskRepository(JpaTransactionManager transactionManager) {
    super(transactionManager);
  }

  public TaskEnumerableResult enumerableResultFilteredByWaitingState() {

    TaskEnumerableResult result =
        new TaskEnumerableResult(this.transactionManager, TaskState.waiting);

    return result;
  }

  public Task getByTag(EntityManager em, String routerId, String tag)
      throws NotFoundException {

    Task entity = (Task) em.createQuery(
        "select t from Task t where t.tag = :tag and t.router.id = :routerId")
        .setParameter("tag", tag)
        .setParameter("routerId", routerId)
        .getSingleResult();

    if (entity != null) {
      return entity;
    }

    throw new NotFoundException("Task with tag: '" + tag + "' not found");
  }

}
