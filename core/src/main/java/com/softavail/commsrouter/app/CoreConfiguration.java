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

/**
 * Created by @author mapuo on 27.10.17.
 */
public interface CoreConfiguration {

  CoreConfiguration DEFAULT = new CoreConfiguration() {
    @Override
    public Integer getBackoffDelay() {
      return 2;
    }

    @Override
    public Integer getBackoffDelayMax() {
      return 60;
    }

    @Override
    public Integer getJitter() {
      return 500;
    }

    @Override
    public Integer getDispatcherThreadPoolSize() {
      return 10;
    }

    @Override
    public Integer getDispatcherThreadShutdownDelay() {
      return 10;
    }

    @Override
    public Integer getQueueProcessRetryDelay() {
      return 10;
    }

    @Override
    public Long getQueueProcessorEvictionDelay() {
      return 10L;
    }

    @Override
    public Integer getJpaLockRetryCount() {
      return 10;
    }

    @Override
    public Boolean getApiEnableExpressionSkillValidation() {
      return false;
    }

    @Override
    public Boolean getApiEnableAgentCapabilitiesValidation() {
      return false;
    }

    @Override
    public Boolean getApiEnableTaskRequirementsValidation() {
      return false;
    }

  };

  Integer getBackoffDelay();

  Integer getBackoffDelayMax();

  Integer getJitter();

  Integer getDispatcherThreadPoolSize();

  Integer getDispatcherThreadShutdownDelay();

  Integer getQueueProcessRetryDelay();

  Long getQueueProcessorEvictionDelay();

  Integer getJpaLockRetryCount();

  Boolean getApiEnableExpressionSkillValidation();

  Boolean getApiEnableAgentCapabilitiesValidation();

  Boolean getApiEnableTaskRequirementsValidation();
}
