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

package com.softavail.commsrouter.domain.dto.mappers;

/**
 *
 * @author ikrustev
 */
public class EntityMappers {

  public final AgentMapper agent;
  public final AttributesMapper attributes;
  public final PlanMapper plan;
  public final QueueMapper queue;
  public final RouterMapper router;
  public final TaskMapper task;
  public final AttributeDomainMapper attributeDomain;
  public final SkillMapper skill;

  public EntityMappers() {
    attributes = new AttributesMapper();
    agent = new AgentMapper(attributes);
    plan = new PlanMapper();
    queue = new QueueMapper();
    router = new RouterMapper();
    task = new TaskMapper(attributes);
    attributeDomain = new AttributeDomainMapper();
    skill = new SkillMapper(attributeDomain);
  }

}
