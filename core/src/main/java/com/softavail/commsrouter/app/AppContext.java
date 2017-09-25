/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.jpa.JpaDbFacade;

/**
 *
 * @author ikrustev
 */
public class AppContext {

  public final JpaDbFacade db;
  public final CommsRouterEvaluator evaluator;
  public final TaskDispatcher taskDispatcher;
  public final EntityMappers entityMapper;

  public AppContext(JpaDbFacade db, CommsRouterEvaluator evaluator, TaskDispatcher taskDispatcher,
      EntityMappers dtoMappers) {
    this.db = db;
    this.evaluator = evaluator;
    this.taskDispatcher = taskDispatcher;
    this.entityMapper = dtoMappers;
  }

}
