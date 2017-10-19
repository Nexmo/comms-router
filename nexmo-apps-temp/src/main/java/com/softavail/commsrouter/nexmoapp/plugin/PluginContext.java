/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.nexmoapp.plugin;

import com.nexmo.client.voice.VoiceClient;
import com.softavail.commsrouter.api.dto.arg.CreateTaskArg;
import com.softavail.commsrouter.api.dto.model.CreatedTaskDto;
import com.softavail.commsrouter.api.dto.model.attribute.AttributeGroupDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.CommsRouterClient;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.dto.mappers.AttributesMapper;
import com.softavail.commsrouter.nexmoapp.api.callback.CommsRouterResource;
import com.softavail.commsrouter.nexmoapp.api.service.ClientService;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.domain.Module;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import com.softavail.commsrouter.nexmoapp.interfaces.SessionService;
import com.softavail.commsrouter.nexmoapp.jpa.TransactionManagerFactory;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Optional;
import javax.ws.rs.core.UriBuilder;

/**
 * @author ikrustev
 */
public class PluginContext {

  private final Configuration configuration;
  private final SessionService sessionService;
  private final Session session;
  private final String routerId;
  private final AttributesMapper attributesMapper;

  public PluginContext(
      final Configuration configuration,
      TransactionManagerFactory transactionManagerFactory,
      final SessionService sessionService,
      final Session session)
      throws CommsRouterException {

    this.configuration = configuration;
    this.sessionService = sessionService;
    this.session = session;
    this.attributesMapper = transactionManagerFactory.getEntityMappers().attributesMapper;
    this.routerId = session.getApplication().getAttributes().getAttributes().stream()
        .filter(this::isRouterIdAttr)
        .map(Attribute::getStringValue)
        .findFirst()
        .orElseThrow(() ->
            new CommsRouterException("Router ID not set in Session Attributes"));
  }

  private boolean isRouterIdAttr(Attribute attribute) {
    return attribute.getName().equals("routerId");
  }

  public CommsRouterClient getCommsClient()
      throws CommsRouterException {

    return ClientService.createCommsClient(routerId, configuration.getCommsRouterUrl());
  }

  public VoiceClient getVoiceClient()
      throws CommsRouterException {

    return ClientService.createVoiceClient(session.getApplication());
  }

  public AttributeGroupDto getApplicationAttributes() {
    return attributesMapper.toDto(session.getApplication().getAttributes());
  }

  public AttributeGroupDto getSessionAttributes() {
    return attributesMapper.toDto(session.getAttributes());
  }

  /**
   * Creates a new Task in the Comms Router
   *
   * It is required to have 'routerId' attribute in the Session Attributes with the Id of the
   * router
   *
   * @param createTaskArg the arguments to source the Task
   * @return the Task Id and the size of the Queue where the task is at moment of creation
   * @throws CommsRouterException when creating the Task
   */
  public CreatedTaskDto createTask(CreateTaskArg createTaskArg)
      throws CommsRouterException {

    if (createTaskArg.getCallbackUrl() == null) {
      // Fill the callback URL
      try {
        URL callbackUrl = UriBuilder.fromUri(configuration.getCallbackBaseUrl())
            .path("/callback")
            .path(CommsRouterResource.class, "taskAssignedWithSession")
            .build(session.getApplication().getId(), session.getId())
            .toURL();
        createTaskArg.setCallbackUrl(callbackUrl);

      } catch (MalformedURLException e) {

        throw new CommsRouterException("Exception when constructing callback URL");
      }
    }

    return getCommsClient().getTaskClient().create(createTaskArg, routerId);
  }

  /**
   * Saves the attributes in the current session
   *
   * @param attributes the modified attributes
   * @throws CommsRouterException when saving to the DB
   */
  public void saveSession(AttributeGroupDto attributes)
      throws CommsRouterException {

    sessionService.update(session.getId(), attributesMapper.toJpa(attributes));
  }

  /**
   * The plugin is marked as completed and we continue to the next if any
   *
   * @throws CommsRouterException when saving to the DB
   */
  public void complete()
      throws CommsRouterException {

    Optional<Module> module = getNextModule(session);
    if (module.isPresent()) {
      sessionService.update(session.getId(), module.get());
    }
  }

  /**
   * Saves the attributes of the current session and marks the plugin as completed
   *
   * @param attributes the modified attributes to be saved
   * @throws CommsRouterException when saving to the DB
   */
  public void complete(AttributeGroupDto attributes)
      throws CommsRouterException {

    if (attributes != null) {
      saveSession(attributes);
    }
    complete();
  }

  private Optional<Module> getNextModule(Session session) {
    List<Module> modules = session.getApplication().getModules();
    int indexOf = modules.indexOf(session.getCurrentModule());
    if (indexOf < modules.size()) {
      Module module = modules.get(indexOf + 1);
      return Optional.of(module);
    }
    return Optional.empty();
  }

}
