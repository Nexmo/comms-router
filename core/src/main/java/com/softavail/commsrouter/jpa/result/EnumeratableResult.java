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
