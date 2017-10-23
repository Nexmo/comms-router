package com.softavail.commsrouter.app;

import com.google.common.collect.Maps;

import com.softavail.commsrouter.api.interfaces.TaskEventHandler;
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

  private static final long EVICTION_DELAY_MINUTES = 10;
  private static final boolean DO_NOT_INTERRUPT_IF_RUNNING = false;

  private static final QueueProcessorManager instance = new QueueProcessorManager();

  private final Map<String, QueueProcessor> QUEUE_PROCESSORS = Maps.newHashMap();
  private final Map<String, ScheduledFuture> SCHEDULED_FUTURES = Maps.newHashMap();

  private QueueProcessorManager() {
  }

  public static QueueProcessorManager getInstance() {
    return instance;
  }

  public synchronized void processQueue(String queueId, JpaDbFacade db,
      EntityMappers mappers, TaskDispatcher taskDispatcher, TaskEventHandler taskEventHandler,
      ScheduledThreadPoolExecutor threadPool) {

    Optional.ofNullable(SCHEDULED_FUTURES.get(queueId))
        .ifPresent(scheduledFuture -> scheduledFuture.cancel(DO_NOT_INTERRUPT_IF_RUNNING));

    QueueProcessor queueProcessor = QUEUE_PROCESSORS.get(queueId);
    if (queueProcessor == null) {
      queueProcessor = new QueueProcessor.Builder()
          .setQueueId(queueId)
          .setDb(db)
          .setMappers(mappers)
          .setTaskDispatcher(taskDispatcher)
          .setTaskEventHandler(taskEventHandler)
          .setThreadPool(threadPool)
          .setStateChangeListener((StateIdleListener) processedQueueId -> {
            ScheduledFuture<?> schedule = threadPool.schedule(() ->
                removeQueueProcessor(processedQueueId), EVICTION_DELAY_MINUTES, TimeUnit.MINUTES);
            SCHEDULED_FUTURES.put(processedQueueId, schedule);
          })
          .build();
      QUEUE_PROCESSORS.put(queueId, queueProcessor);
    }
    queueProcessor.process();
  }

  private synchronized void removeQueueProcessor(String queueId) {
    QueueProcessor queueProcessor = QUEUE_PROCESSORS.get(queueId);
    if (!queueProcessor.isWorking()) {
      QUEUE_PROCESSORS.remove(queueId);
    }
  }

}
