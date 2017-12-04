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

import com.google.common.collect.Maps;

import com.softavail.commsrouter.domain.dto.mappers.EntityMappers;
import com.softavail.commsrouter.jpa.JpaDbFacade;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Created by @author mapuo on 23.10.17.
 */
public class QueueProcessorManager {

  private static final boolean DO_NOT_INTERRUPT_IF_RUNNING = false;

  private static final QueueProcessorManager instance = new QueueProcessorManager();

  private final Map<Long, QueueProcessor> queueProcessors = Maps.newHashMap();
  private final Map<Long, ScheduledFuture> scheduledFutures = Maps.newHashMap();

  private QueueProcessorManager() {
  }

  public static QueueProcessorManager getInstance() {
    return instance;
  }

  public synchronized void processQueue(Long routerId, Long queueId,
      JpaDbFacade db,
      EntityMappers mappers,
      TaskDispatcher taskDispatcher,
      CoreConfiguration configuration,
      ScheduledThreadPoolExecutor threadPool) {

    Optional.ofNullable(scheduledFutures.get(queueId))
        .ifPresent(scheduledFuture -> scheduledFuture.cancel(DO_NOT_INTERRUPT_IF_RUNNING));

    QueueProcessor queueProcessor = queueProcessors.get(queueId);
    if (queueProcessor == null) {
      queueProcessor = new QueueProcessor.Builder()
          .setRouterId(routerId)
          .setQueueId(queueId)
          .setDb(db)
          .setTaskDispatcher(taskDispatcher)
          .setThreadPool(threadPool)
          .setProcessRetryDelaySeconds(configuration.getQueueProcessRetryDelay())
          .setStateChangeListener((StateIdleListener) processedQueueId -> {
            ScheduledFuture<?> schedule = threadPool.schedule(
                () -> removeQueueProcessor(processedQueueId),
                configuration.getQueueProcessorEvictionDelay(), TimeUnit.MINUTES);
            scheduledFutures.put(processedQueueId, schedule);
          })
          .build();
      queueProcessors.put(queueId, queueProcessor);
    }
    queueProcessor.process();
  }

  private synchronized void removeQueueProcessor(Long queueId) {
    QueueProcessor queueProcessor = queueProcessors.get(queueId);
    if (!queueProcessor.isWorking()) {
      queueProcessors.remove(queueId);
    }
  }

}
