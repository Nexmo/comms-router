/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.nexmoapp.plugin;

import com.google.common.collect.ImmutableList;
import com.nexmo.client.voice.VoiceClient;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.CommsRouterClient;
import com.softavail.commsrouter.nexmoapp.api.callback.CommsRouterResource;
import com.softavail.commsrouter.nexmoapp.api.service.ClientService;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.domain.Attribute;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.domain.Session;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import javax.ws.rs.core.UriBuilder;

/**
 * @author ikrustev
 */
public class PluginContext {

  // private final Application application; // Immutable Settings
  private final Session session; // Mutable Settings
  private final Configuration configuration;

  public PluginContext(final Session session, final Configuration configuration) {
    this.session = session;
    this.configuration = configuration;
  }

  public CommsRouterClient getCommsClient()
      throws CommsRouterException {

    return ClientService.createCommsClient(session.getApplication());
  }

  public VoiceClient getVoiceClient()
      throws CommsRouterException {

    return ClientService.createVoiceClient(session.getApplication());
  }

  public ImmutableList<Attribute> getApplicationAttributes() {
    return ImmutableList.copyOf(session.getApplication().getAttributes().getAttributes());
  }

  public List<Attribute> getSessionAttributes() {
    return session.getAttributes().getAttributes();
  }

  public CreatedTaskDto createdTask(CreateTaskArg taskDto)
      throws CommsRouterException {

    String routerId = ""; // TODO

    if (taskDto.getCallbackUrl() == null) {
      // Fill the callback URL
      try {
        URL callbackUrl = UriBuilder.fromUri(configuration.getCallbackBaseUrl())
            .path("/callback")
            .path(CommsRouterResource.class, "taskAssignedWithSession")
            .build(session.getApplication().getId(), session.getId())
            .toURL();
        taskDto.setCallbackUrl(callbackUrl);

      } catch (MalformedURLException e) {

        throw new CommsRouterException("Exception when constructing callback URL");
      }
    }

    return getCommsClient().getTaskClient().create(taskDto, routerId);
  }

  public void markDone() {
    List<Module> modules = session.getApplication().getModules();
    int indexOf = modules.indexOf(session.getCurrentModule());
    if (indexOf < modules.size()) {
      Module module = modules.get(indexOf + 1);
      session.setCurrentModule(module);
    }
  }

}
