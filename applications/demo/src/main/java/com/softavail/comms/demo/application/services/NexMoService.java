package com.softavail.comms.demo.application.services;

import com.nexmo.client.voice.ModifyCallResponse;
import com.nexmo.client.voice.VoiceClient;

/**
 * Created by @author mapuo on 29.08.17.
 */
public interface NexMoService {

  VoiceClient getVoiceClient();

  ModifyCallResponse transferCall(String id, String destinationUrl);

}
