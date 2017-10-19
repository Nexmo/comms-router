package com.softavail.commsrouter.nexmoapp.api.service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import com.nexmo.client.NexmoClient;
import com.nexmo.client.auth.AuthMethod;
import com.nexmo.client.auth.JWTAuthMethod;
import com.nexmo.client.voice.VoiceClient;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.client.CommsRouterClient;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.nexmoapp.config.Configuration;
import com.softavail.commsrouter.nexmoapp.domain.Application;
import com.softavail.commsrouter.nexmoapp.domain.Session;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class ClientService {

  private static final Client client;

  static {
    ClientConfig config = new ClientConfig();
    config.property(ClientProperties.CONNECT_TIMEOUT, 1500);
    config.property(ClientProperties.READ_TIMEOUT, 1500);
    config.register(new LoggingFeature());
    client = ClientBuilder.newClient(config);
  }

  // TODO Cache the client(s)
  private static final LoadingCache<CommsKey, CommsRouterClient> COMMS_ROUTER_CLIENT =
      CacheBuilder.newBuilder()
          .maximumSize(20)
          .expireAfterAccess(1, TimeUnit.MINUTES)
          .build(new CacheLoader<CommsKey, CommsRouterClient>() {
            @Override
            public CommsRouterClient load(CommsKey key) throws Exception {
              return new CommsRouterClient(client, key.getCommsRouterUrl(), key.getRouterId());
            }
          });

  private static final LoadingCache<Application, VoiceClient> NEXMO_VOICE_CLIENT =
      CacheBuilder.newBuilder()
          .maximumSize(20)
          .expireAfterAccess(1, TimeUnit.MINUTES)
          .build(new CacheLoader<Application, VoiceClient>() {
            @Override
            public VoiceClient load(Application application) throws Exception {
              String applicationId = application.getNexmoAppId();
              byte[] privateKey = application.getPrivateKey().getBytes(StandardCharsets.UTF_8);
              AuthMethod jwtAuth = new JWTAuthMethod(applicationId, privateKey);
              NexmoClient nexmoClient = new NexmoClient(jwtAuth);
              return nexmoClient.getVoiceClient();
            }
          });

  public static VoiceClient createVoiceClient(Application application)
      throws CommsRouterException {

    try {

      return NEXMO_VOICE_CLIENT.get(application);

    } catch (ExecutionException e) {

      throw new CommsRouterException("Invalid credentials. Can't create client.");
    }
  }

  public static CommsRouterClient createCommsClient(String commsRouterUrl, String routerId)
      throws CommsRouterException {

    try {

      return COMMS_ROUTER_CLIENT.get(new CommsKey(commsRouterUrl, routerId));

    } catch (ExecutionException e) {

      throw new CommsRouterException("Invalid credentials. Can't create client.");
    }
  }

  private static class CommsKey {

    private String commsRouterUrl;
    private String routerId;

    public CommsKey() {
    }

    public CommsKey(String commsRouterUrl, String routerId) {
      this.commsRouterUrl = commsRouterUrl;
      this.routerId = routerId;
    }

    public CommsKey(Session session, Configuration configuration)
        throws CommsRouterException {

      this.commsRouterUrl = configuration.getCommsRouterUrl();
      this.routerId = session.getAttributes().getAttributes().stream()
          .filter(CommsKey::isRouterIdAttr)
          .map(Attribute::getStringValue)
          .findFirst()
          .orElseThrow(() ->
              new CommsRouterException("Router ID not set in Session Attributes"));
    }

    private static boolean isRouterIdAttr(Attribute attribute) {
      return attribute.getName().equals("routerId");
    }

    public String getCommsRouterUrl() {
      return commsRouterUrl;
    }

    public void setCommsRouterUrl(String commsRouterUrl) {
      this.commsRouterUrl = commsRouterUrl;
    }

    public String getRouterId() {
      return routerId;
    }

    public void setRouterId(String routerId) {
      this.routerId = routerId;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null || getClass() != obj.getClass()) {
        return false;
      }
      CommsKey commsKey = (CommsKey) obj;
      return Objects.equals(getCommsRouterUrl(), commsKey.getCommsRouterUrl())
          && Objects.equals(getRouterId(), commsKey.getRouterId());
    }

    @Override
    public int hashCode() {
      return Objects.hash(getCommsRouterUrl(), getRouterId());
    }

  }

}
