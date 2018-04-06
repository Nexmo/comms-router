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
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.persistence.EntityManager;

/**
 * @author ikrustev
 */
public class CorePlanService extends CoreRouterObjectService<PlanDto, Plan> implements PlanService {

  static final int INITIAL_REVISION = 1;

  public CorePlanService(AppContext app) {
    super(app, app.db.plan, app.entityMapper.plan);
  }

  @Override
  public ApiObjectRef create(CreatePlanArg createArg, String routerId) throws CommsRouterException {

    RouterObjectRef routerObjectRef =
        RouterObjectRef.builder().setRef(Uuid.get()).setRouterRef(routerId).build();

    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, routerObjectRef, INITIAL_REVISION);
    });
  }

  @Override
  public ApiObjectRef replace(CreatePlanArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      Plan oldPlan = app.db.plan.delete(em, objectRef);
      int revision = calculateNextRevision(oldPlan);
      em.flush();
      return doCreate(em, createArg, objectRef, revision);
    });
  }

  private static int calculateNextRevision(Plan oldPlan) {
    return oldPlan == null ? INITIAL_REVISION : oldPlan.getRevision() + 1;
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    if (updateArg.canDoNoBackupUpdate()) {
      app.db.transactionManager.executeVoid((em) -> {
        Plan plan = app.db.plan.get(em, objectRef);
        checkResourceVersion(plan, objectRef);
        Fields.update(plan::setDescription, plan.getDescription(), updateArg.getDescription());
      });
      return;
    }

    // Create a copy of the plan and keep the old one as a backup, but under different ref

    app.db.transactionManager.executeVoid((em) -> {
      Plan oldPlan = app.db.plan.get(em, objectRef);
      checkResourceVersion(oldPlan, objectRef);
      int revision = calculateNextRevision(oldPlan);
      PlanDto oldDto = app.entityMapper.plan.toDto(oldPlan);
      CreatePlanArg createArg = prepareCreateCopyArg(oldDto, updateArg);
      oldPlan.markBackup(updateArg.getDescription());
      em.flush();
      doCreate(em, createArg, objectRef, revision);
    });
  }

  private <T> T getFirstNonNull(T first, T second) {
    return first != null ? first : second;
  }

  private CreatePlanArg prepareCreateCopyArg(PlanDto oldDto, UpdatePlanArg updateArg) {
    CreatePlanArg createArg = new CreatePlanArg();
    createArg.setDescription(getFirstNonNull(updateArg.getDescription(), oldDto.getDescription()));
    createArg.setRules(getFirstNonNull(updateArg.getRules(), oldDto.getRules()));
    createArg.setDefaultRoute(getFirstNonNull(
            updateArg.getDefaultRoute(), oldDto.getDefaultRoute()));
    return createArg;
  }

  private ApiObjectRef doCreate(EntityManager em, CreatePlanArg createArg,
      RouterObjectRef objectRef, int revision)
      throws CommsRouterException {

    if (createArg.getDefaultRoute() == null) {
      throw new IllegalArgumentException(
          "Field 'defaultRoute' is required for plan creation.");
    }

    if (createArg.getDefaultRoute().getQueueRef() == null) {
      throw new IllegalArgumentException("Field 'queueRef' is required in the default route.");
    }

    if (createArg.getRules() != null) {
      for (RuleDto rule : createArg.getRules()) {
        app.evaluatorFactory.provide(rule.getPredicate(), objectRef.getRouterRef()).validate();
      }
    }

    Router router = getRouter(em, objectRef);
    Plan plan = new Plan(objectRef);
    plan.setRouter(router);
    plan.setDescription(createArg.getDescription());
    plan.setRevision(revision);

    PlanResolver.create(app, em, plan).addDtoRules(createArg.getRules())
            .setDefaultDtoRoute(createArg.getDefaultRoute());
    em.persist(plan);
    PlanDto planDto = entityMapper.toDto(plan);
    return new ApiObjectRef(planDto);
  }

}
