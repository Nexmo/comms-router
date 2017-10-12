/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */
package com.softavail.commsrouter.nexmoapp.plugin;

import com.nexmo.client.voice.VoiceClient;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.CommsRouterClient;
import com.softavail.commsrouter.nexmoapp.api.service.ClientService;
import com.softavail.commsrouter.nexmoapp.domain.Session;

/**
 * @author ikrustev
 */
public class PluginContext {

  // private final Application application; // Immutable Settings
  private final Session session; // Mutable Settings
  private VoiceClient voiceClient;
  private CommsRouterClient commsClient;

  public PluginContext(final Session session)
      throws CommsRouterException {

    this.session = session;
  }

  public CommsRouterClient getCommsClient()
      throws CommsRouterException {

    if (commsClient == null) {
      commsClient = ClientService.createCommsClient(session.getApplication());
    }

    return commsClient;
  }

  public VoiceClient getVoiceClient()
      throws CommsRouterException {

    // Add nexmoClient with already created JWT
    if (voiceClient == null) {
      voiceClient = ClientService.createVoiceClient(session.getApplication());
    }

    return voiceClient;
  }

  // TODO Add createTask() > set URLs
  // TODO method imFinished > getNextModule > assign to Session

  public CreatedTaskDto createdTask(CreateTaskArg taskDto)
      throws CommsRouterException {

    String routerId = ""; // TODO
    return commsClient.getTaskService(routerId).create(taskDto, routerId);
  }

}
