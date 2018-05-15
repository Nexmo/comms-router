/*
 * Copyright 2018 SoftAvail Inc.
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

import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.app.AppContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author vladislav
 */
public class Validators {

  private static final Logger LOGGER = LogManager.getLogger(Validators.class);

  public final SkillValidator taskRequirementsValidator;
  public final SkillValidator agentCapabilitiesValidator;

  public Validators(AppContext context) {
    taskRequirementsValidator = getTaskRequirementsValidator(context);
    agentCapabilitiesValidator = getAgentCapabilitiesValidator(context);
  }

  private SkillValidator getAgentCapabilitiesValidator(AppContext context) {

    LOGGER.info("api.enableAgentCapabilitiesValidation: {}",
        context.coreConfiguration.getApiEnableAgentCapabilitiesValidation());

    if (context.coreConfiguration.getApiEnableAgentCapabilitiesValidation()) {
      return new SkillValidator(context.svc.skill);
    } else {
      return new SkillValidator(null) {
        @Override
        public void validate(AttributeGroupDto capabilities, String routerRef)
            throws CommsRouterException {

          // validate skill name
          if (capabilities != null) {
            for (String skillName : capabilities.keySet()) {
                context.evaluatorFactory.validateRsqlSelector(skillName);
            }
          }

        }
      };
    }
  }

  private SkillValidator getTaskRequirementsValidator(AppContext context) {

    LOGGER.info("api.enableTaskRequirementsValidation: {}",
        context.coreConfiguration.getApiEnableTaskRequirementsValidation());

    if (context.coreConfiguration.getApiEnableTaskRequirementsValidation()) {
      return new SkillValidator(context.svc.skill);
    } else {
      return new SkillValidator(null) {
        @Override
        public void validate(AttributeGroupDto capabilities, String routerRef)
            throws CommsRouterException {

          // validate skill name
          if (capabilities != null) {
            for (String skillName : capabilities.keySet()) {
                context.evaluatorFactory.validateRsqlSelector(skillName);
            }
          }

        }
      };
    }
  }
}
