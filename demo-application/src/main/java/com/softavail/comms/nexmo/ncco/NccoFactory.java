package com.softavail.comms.nexmo.ncco;

import java.util.ArrayList;
import java.util.List;

import com.nexmo.client.voice.ncco.InputNcco;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.softavail.comms.demo.application.model.ConversationNccoEx;

public class NccoFactory {

  public List<Ncco> nccoListWithPromptCallback(String text, String eventUrl) {
    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(true);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(5);
   
    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);
    
    return list;
  }

  public List<Ncco> nccoListWithPromptCallerId(String text, String eventUrl) {
    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(true);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(5);
   
    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);
    
    return list;
  }

  public List<Ncco> nccoListWithGetNumber(String text, String eventUrl) {

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

  public List<Ncco> nccoListWithConfirmNumber(String text, String eventUrl) {

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

  public List<Ncco> nccoListWithAnswerFromAgentForCallbackTask(
      String text, 
      String conversationId,
      String musicOnHoldUrl) {

    TalkNcco talk = new TalkNcco(text);

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setMusicOnHoldUrl(musicOnHoldUrl);
    conv.setStartOnEnter(false);
    conv.setRecord(false);
   
    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(conv);
    
    return list;
  }

  public List<Ncco> nccoListWithAnswerFromCustomerForCallbackTask(
      String text, 
      String conversationId,
      String musicOnHoldUrl) {

    TalkNcco talk = new TalkNcco(text);

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setRecord(false);
   
    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(conv);
    
    return list;
  }
  
}
