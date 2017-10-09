package com.softavail.comms.nexmo.ncco;

import java.util.ArrayList;
import java.util.List;

import com.nexmo.client.voice.ncco.InputNcco;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.softavail.comms.demo.application.model.ConversationNccoEx;

public class NccoFactory {

  public List<Ncco> nccoListWithOfferCallback(String text, String eventUrl) {
    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(true);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(10);
   
    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);
    
    return list;
  }

  public List<Ncco> nccoListWithChangeNumber(String text, String eventUrl) {

    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(true);
    
    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(15);
    input.setSubmitOnHash(true);
    input.setMaxDigits(20);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);
    
    return list;
  }

  public Ncco nccoTalkWithRegularTaskGreeting(String text) {
    TalkNcco ncco = new TalkNcco(text);
    return ncco;
  }


  public Ncco nccoConversationWithRegularTask(String conversationId,
      String musicOnHoldUrl) {
    
    ConversationNccoEx ncco = new ConversationNccoEx(conversationId);
    ncco.setMusicOnHoldUrl(musicOnHoldUrl);
    ncco.setStartOnEnter(false);
    ncco.setRecord(false);

    return ncco;
  }
  
}
