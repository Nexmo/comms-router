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

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.RuleDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CorePlanService extends CoreRouterObjectService<PlanDto, Plan> implements PlanService {

  public CorePlanService(AppContext app) {
    super(app, app.db.plan, app.entityMapper.plan);
  }

  @Override
  public ApiObjectRef create(CreatePlanArg createArg, String routerId) throws CommsRouterException {

    RouterObjectRef routerObjectRef =
        RouterObjectRef.builder().setRef(Uuid.get()).setRouterRef(routerId).build();

    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, routerObjectRef);
    });
  }

  @Override
  public ApiObjectRef replace(CreatePlanArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.plan.delete(em, objectRef);
      em.flush();
      return doCreate(em, createArg, objectRef);
    });
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Plan plan = app.db.plan.get(em, objectRef);
      PlanResolver planResolver = PlanResolver.create(app, em, plan);
      Fields.update(plan::setDescription, plan.getDescription(), updateArg.getDescription());
    });
  }

  private ApiObjectRef doCreate(EntityManager em, CreatePlanArg createArg,
      RouterObjectRef objectRef)
      throws CommsRouterException {

    if (createArg.getDefaultRoute() == null) {
      throw new IllegalArgumentException(
          "Default route 'default_route' is mandatory option for plan creation.");
    }

    if (createArg.getDefaultRoute().getQueueId() == null) {
      throw new IllegalArgumentException("Queue ID 'queueId' is required in the default route.");
    }

    CommsRouterEvaluator evaluator = app.evaluatorFactory.provide(null);
    if (createArg.getRules() != null) {
      for (RuleDto rule : createArg.getRules()) {
        evaluator.validate(rule.getPredicate());
      }
    }

    Router router = getRouter(em, objectRef);
    Plan plan = new Plan(objectRef);
    plan.setRouter(router);
    plan.setDescription(createArg.getDescription());

    PlanResolver.create(app, em, plan).addDtoRules(createArg.getRules())
            .setDefaultDtoRoute(createArg.getDefaultRoute());
    em.persist(plan);
    PlanDto planDto = entityMapper.toDto(plan);
    return new ApiObjectRef(planDto);
  }

}
