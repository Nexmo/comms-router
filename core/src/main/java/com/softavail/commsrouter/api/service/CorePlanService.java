/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.dto.arg.CreatePlanArg;
import com.softavail.commsrouter.api.dto.arg.UpdatePlanArg;
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
  public PlanDto create(CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    objectId.setId(Uuid.get());
    return app.db.transactionManager.execute((em) -> {
      return doCreate(em, createArg, objectId);
    });
  }

  @Override
  public PlanDto replace(CreatePlanArg createArg, RouterObjectId objectId)
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
      Fields.update(plan::setDescription, plan.getDescription(), updateArg.getDescription());
    });
  }

  private PlanDto doCreate(EntityManager em, CreatePlanArg createArg, RouterObjectId objectId)
      throws CommsRouterException {

    Plan plan = new Plan(createArg, objectId);
    app.entityMapper.plan.addDtoRules(plan, createArg.getRules());
    em.persist(plan);
    return entityMapper.toDto(plan);
  }

}
