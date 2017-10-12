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
import com.softavail.commsrouter.nexmoapp.domain.Application;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * Created by @author mapuo on 12.10.17.
 */
public class ClientService {

  // TODO Cache the client(s)
  private static final LoadingCache<Application, CommsRouterClient> COMMS_ROUTER_CLIENT =
      CacheBuilder.newBuilder()
          .maximumSize(20)
          .expireAfterAccess(1, TimeUnit.MINUTES)
          .build(new CacheLoader<Application, CommsRouterClient>() {
            @Override
            public CommsRouterClient load(Application key) throws Exception {
              return null; // TODO
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

  public static CommsRouterClient createCommsClient(Application application)
      throws CommsRouterException {

    try {

      return COMMS_ROUTER_CLIENT.get(application);

    } catch (ExecutionException e) {

      throw new CommsRouterException("Invalid credentials. Can't create client.");
    }
  }
}
