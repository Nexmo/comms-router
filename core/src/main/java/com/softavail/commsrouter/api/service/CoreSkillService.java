/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.arg.CreateSkillArg;
import com.softavail.commsrouter.api.dto.arg.UpdateSkillArg;
import com.softavail.commsrouter.api.dto.model.ApiObjectRef;
import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.exception.BadValueException;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.interfaces.SkillService;
import com.softavail.commsrouter.app.AppContext;
import com.softavail.commsrouter.domain.Router;
import com.softavail.commsrouter.domain.Skill;
import com.softavail.commsrouter.util.Fields;
import com.softavail.commsrouter.util.Uuid;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.persistence.EntityManager;

/**
 *
 * @author ikrustev
 */
public class CoreSkillService extends CoreRouterObjectService<SkillDto, Skill>
    implements SkillService {

  private static final Logger LOGGER = LogManager.getLogger(CoreSkillService.class);

  public CoreSkillService(AppContext app) {
    super(app, app.db.skill, app.entityMapper.skill);
  }

  @Override
  public ApiObjectRef create(CreateSkillArg createArg, String routerRef)
      throws CommsRouterException {

    RouterObjectRef routerObjectRef =
        RouterObjectRef.builder().setRef(Uuid.get()).setRouterRef(routerRef).build();

    return app.db.transactionManager.execute((EntityManager em) -> {
      return doCreate(em, createArg, routerObjectRef);
    });
  }

  @Override
  public ApiObjectRef replace(CreateSkillArg createArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    return app.db.transactionManager.execute((em) -> {
      app.db.skill.delete(em, objectRef);
      em.flush();
      return doCreate(em, createArg, objectRef);
    });
  }

  private ApiObjectRef doCreate(EntityManager em, CreateSkillArg createArg,
      RouterObjectRef objectRef)
      throws CommsRouterException {

    app.evaluatorFactory.validateRsqlSelector(objectRef.getRef());

    if (createArg.getDomain() == null) {
      throw new BadValueException("Field 'domain' is required.");
    }
    createArg.getDomain().validate();

    Router router = getRouter(em, objectRef);
    Skill skill = new Skill(objectRef);
    skill.setRouter(router);
    skill.setDescription(createArg.getDescription());
    skill.setMultivalue(createArg.getMultivalue());
    skill.setDomain(app.entityMapper.attributeDomain.fromDto(createArg.getDomain()));
    em.persist(skill);
    return skill.cloneApiObjectRef();
  }

  @Override
  public void update(UpdateSkillArg updateArg, RouterObjectRef objectRef)
      throws CommsRouterException {

    if (updateArg.getDomain() != null) {
      updateArg.getDomain().validate();
    }

    app.db.transactionManager.executeVoid((em) -> {
      Skill skill = app.db.skill.get(em, objectRef);
      checkResourceVersion(skill, objectRef);
      Fields.update(skill::setDescription, skill.getDescription(), updateArg.getDescription());
      Fields.update(skill::setMultivalue, skill.getMultivalue(), updateArg.getMultivalue());
      if (updateArg.getDomain() != null) {
        skill.setDomain(app.entityMapper.attributeDomain.fromDto(updateArg.getDomain()));
      }
    });
  }

}
