/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectId;
import com.softavail.commsrouter.api.dto.model.PlanDto;
import com.softavail.commsrouter.api.dto.model.RouterObjectId;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.PlanService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Plan;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;

import javax.persistence.EntityManager;



/**
 * @author ikrustev
 */
public class CorePlanService extends CoreRouterObjectService<PlanDto, Plan>
    implements PlanService {

  public CorePlanService(AppContext app) {
    super(app, app.db.plan, app.entityMapper.plan);
  }

  @Override
  public ApiObjectId create(CreatePlanArg createArg, String routerId)
      throws CommsRouterException {

    RouterObjectId routerObjectId = RouterObjectId.builder()
        .setId(Uuid.get())
        .setRouterId(routerId)
        .build();

    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, routerObjectId);
    });
  }

  @Override
  public ApiObjectId create(CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.plan.delete(em, objectId.getId());
      return doCreate(em, createArg, objectId);
    });
  }

  @Override
  public void update(UpdatePlanArg updateArg, RouterObjectId objectId)
      throws CommsRouterException {

    app.db.transactionManager.executeVoid((em) -> {
      Plan plan = app.db.plan.get(em, objectId.getId());
      if (updateArg.getRules() != null) {
        plan.removeRules();
        app.entityMapper.plan.addDtoRules(plan, updateArg.getRules());
      }
      Fields.update(plan::setDefaultRoute, plan.getDefaultRoute(),
          app.entityMapper.plan.fromRouteDto(updateArg.getDefaultRoute()));
      Fields.update(plan::setDescription, plan.getDescription(), updateArg.getDescription());
    });
  }

  private ApiObjectId doCreate(EntityManager em, CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    if (createArg.getDefaultRoute() == null) {
      throw new IllegalArgumentException(
          "Default route 'default_route' is mandatory option for plan creation.");
    }

    if (createArg.getDefaultRoute().getQueueId() == null) {
      throw new IllegalArgumentException("Queue ID 'queueId' is required in the default route.");
    }

    Plan plan = new Plan(createArg, objectId);
    app.entityMapper.plan.addDtoRules(plan, createArg.getRules());
    plan.setDefaultRoute(app.entityMapper.plan.fromRouteDto(createArg.getDefaultRoute()));
    em.persist(plan);
    PlanDto planDto = entityMapper.toDto(plan);
    return new ApiObjectId(planDto);
  }

}
