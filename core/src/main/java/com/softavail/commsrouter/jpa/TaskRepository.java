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
        "select t from Task t where t.tag = :tag and t.router.ref = :routerId")
        .setParameter("tag", tag)
        .setParameter("routerId", routerId)
        .getSingleResult();

    if (entity != null) {
      return entity;
    }

    throw new NotFoundException("Task with tag: '" + tag + "' not found");
  }

}
