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

package com.softavail.commsrouter.app;

import com.softavail.commsrouter.api.service.Services;
import com.softavail.commsrouter.api.service.Validators;
import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.eval.CommsRouterEvaluatorFactory;
import com.softavail.commsrouter.jpa.JpaDbFacade;

/**
 *
 * @author ikrustev
 */
public class AppContext {

  public final JpaDbFacade db;
  public final CommsRouterEvaluatorFactory evaluatorFactory;
  public final TaskDispatcher taskDispatcher;
  public final EntityMappers entityMapper;
  public final CoreConfiguration coreConfiguration;
  public final Services svc;
  public final Validators validators;

  public AppContext(JpaDbFacade db, CommsRouterEvaluatorFactory evaluatorFactory,
      TaskDispatcher taskDispatcher, EntityMappers dtoMappers,
      CoreConfiguration coreConfiguration) {
    this.db = db;
    this.evaluatorFactory = evaluatorFactory;
    this.taskDispatcher = taskDispatcher;
    this.entityMapper = dtoMappers;
    this.coreConfiguration = coreConfiguration;
    this.svc = new Services(this);
    this.validators = new Validators(this);
  }

}
