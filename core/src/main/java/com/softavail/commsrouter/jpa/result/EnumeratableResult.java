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

import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.jpa.JpaTransactionManager;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.Query;

public abstract class EnumeratableResult<T> implements AutoCloseable {

  protected JpaTransactionManager transactionManager;
  //TODO: Make maxResults 1000 when a sequental id is introduced in task table
  // and change the db query to reflect sequental numbers in order to process each waiting task 
  protected int maxResults = 1000000;
  protected List<T> currentRow;
  
  private int offset = 0;
  private boolean reachedEnd = false;
  
  public EnumeratableResult(JpaTransactionManager tm, int maxResults) {
    this.transactionManager = tm;
    this.maxResults = maxResults;
  }
  
  protected abstract Query createQuery(EntityManager em);
  
  /**
   * Release resources immediately.
   */
  @Override
  public void close() throws Exception {
    // TODO Auto-generated method stub
  }

  /**
   * Advance to the next result.
   *
   * @return {@code true} if there is another result
   */
  @SuppressWarnings("unchecked")
  public boolean next() {
    
    if (this.reachedEnd == true) { 
      this.currentRow = null;
      return false;
    }
    
    boolean hasNext;
    List<T> entities = null;
    
    try {
      entities = this.transactionManager.execute((em) -> {
        List<T> list = null;
        Query query = createQuery(em);
        
        if (null != query) {
          list = query.setFirstResult(this.offset)
              .setMaxResults(this.maxResults)
              .getResultList();
        }
        return list;
      });
    } catch (CommsRouterException e) {
      entities = null;
    }
    
    if (null != entities) {
      this.offset = this.offset + entities.size();
      
      if (entities.size() < this.maxResults) {
        reachedEnd = true;
      }
      
      hasNext = (entities.size() > 0);
    } else {
      hasNext = false;
      reachedEnd = true;
    }

    this.currentRow = entities;
    
    return hasNext;
  }

  /**
   * Get the current row of results.
   *
   * @return The array of results
   */
  public List<T> get() {
    return this.currentRow;
  }
}
