/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.nexmoapp.plugin;

import com.softavail.commsrouter.api.dto.model.TaskAssignmentDto;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.domain.Session;

/**
 *
 * @author ikrustev
 */
public interface Plugin {

  Session handleEvent(Application application, NexmoCallEvent event);

  Session handleEvent(Application application, TaskAssignmentDto taskAssignment);

  void handleEvent(PluginContext context, NexmoCallEvent event);

  void handleEvent(PluginContext context, TaskAssignmentDto taskAssignment);

  void handleEvent(PluginContext context, Timeout timeout);

}
