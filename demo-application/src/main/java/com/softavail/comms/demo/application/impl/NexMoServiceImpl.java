package com.softavail.comms.demo.application.impl;

import com.nexmo.client.NexmoClient;
import com.nexmo.client.auth.JWTAuthMethod;
import com.nexmo.client.voice.ModifyCallResponse;
import com.nexmo.client.voice.VoiceClient;
import com.softavail.comms.demo.application.impl.compat.ModifyCallPayload;
import com.softavail.comms.demo.application.services.Configuration;
import com.softavail.comms.demo.application.services.NexMoService;
import org.apache.http.Header;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;

/**
 * Created by @author mapuo on 29.08.17.
 */
@Singleton
public class NexMoServiceImpl implements NexMoService {

  private static final Logger LOGGER = LogManager.getLogger(NexMoServiceImpl.class);

  private static final String TRANSFER = "transfer";

  private final NexmoClient nexmoClient;
  private final JWTAuthMethod jwtAuthMethod;
  private final Client client;

  @Inject
  public NexMoServiceImpl(Configuration configuration, Client client) {
    jwtAuthMethod = configuration.getJwtAuthMethod();
    this.client = client;

    LOGGER.debug("jwtAuthMethod: {}", jwtAuthMethod);

    nexmoClient = new NexmoClient(jwtAuthMethod);

    LOGGER.debug("client created!");
  }

  @Override
  public VoiceClient getVoiceClient() {
    return nexmoClient.getVoiceClient();
  }

  @Override
  public ModifyCallResponse transferCall(String id, String destinationUrl) {

    LOGGER.debug("Transferring call {} to {}",
        id, destinationUrl);

    String authorization = "Authorization";
    String uri = "https://api.nexmo.com/v1/calls/";
    RequestBuilder requestBuilder = jwtAuthMethod.apply(RequestBuilder.put(uri));
    Header header = requestBuilder.getLastHeader(authorization);

    LOGGER.debug("header: {}", header);

    ModifyCallPayload transferPayload = new ModifyCallPayload(TRANSFER, destinationUrl);
    WebTarget target = client.target(uri);
    ModifyCallResponse response = target
        .request(MediaType.APPLICATION_JSON_TYPE)
        .header(authorization, header.getValue())
        .put(
            Entity.entity(transferPayload, MediaType.APPLICATION_JSON_TYPE),
            ModifyCallResponse.class);

    LOGGER.debug("response: {}", response);

    return response;
  }

}
