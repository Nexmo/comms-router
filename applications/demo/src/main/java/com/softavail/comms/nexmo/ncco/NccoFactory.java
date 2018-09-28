package com.softavail.comms.nexmo.ncco;

import com.nexmo.client.voice.ncco.InputNcco;
import com.nexmo.client.voice.ncco.Ncco;
import com.nexmo.client.voice.ncco.RecordNcco;
import com.nexmo.client.voice.ncco.StreamNcco;
import com.nexmo.client.voice.ncco.TalkNcco;
import com.softavail.comms.demo.application.model.ConversationNccoEx;

import java.util.ArrayList;
import java.util.List;

public class NccoFactory {

  public List<Ncco> nccoListWithPromptCallback(String text, String eventUrl) {
    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(true);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(5);
    input.setMaxDigits(1);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);

    return list;
  }

  public List<Ncco> nccoListWithPromptCallerId(String text, String eventUrl) {
    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(Boolean.TRUE);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(5);
    input.setMaxDigits(1);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);

    return list;
  }

  public List<Ncco> nccoListWithGetNumber(String text, String eventUrl) {

    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(Boolean.TRUE);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(20);
    input.setSubmitOnHash(true);
    input.setMaxDigits(20);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);

    return list;
  }

  public List<Ncco> nccoListWithConfirmNumber(String text, String eventUrl) {

    TalkNcco talk = new TalkNcco(text);
    talk.setBargeIn(Boolean.TRUE);

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(10);
    input.setMaxDigits(1);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(input);

    return list;
  }

  public Ncco nccoTalkWithRegularTaskGreeting(String text) {
    TalkNcco ncco = new TalkNcco(text);
    return ncco;
  }

  public List<Ncco> nccoListWithAnswerFromAgentForCallbackTask(
      String text,
      String conversationId,
      String musicOnHoldUrl) {

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setRecord(Boolean.FALSE);
    conv.setEndOnExit(Boolean.TRUE);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(new TalkNcco(text));
    list.add(conv);

    return list;
  }

  public List<Ncco> nccoListWithAnswerFromAgentForRegularTask(
      String text,
      String conversationId,
      String musicOnHoldUrl) {

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setRecord(Boolean.FALSE);
    //conv.setMusicOnHoldUrl(musicOnHoldUrl); // for attendee

    //conv.setStartOnEnter(Boolean.FALSE); // for attendee
    conv.setEndOnExit(Boolean.TRUE); // for moderator

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(new TalkNcco(text));
    list.add(conv);

    return list;
  }


  public List<Ncco> nccoListWithAnswerFromCustomerForCallbackTask(
      String text,
      String conversationId) {

    TalkNcco talk = new TalkNcco(text);

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setStartOnEnter(Boolean.FALSE);
    conv.setRecord(Boolean.FALSE);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(conv);

    return list;
  }

  public List<Ncco> nccoListWithAnswerFromCustomerForRegularTask(
      String text,
      String conversationId,
      String musicOnHoldUrl) {

    TalkNcco talk = new TalkNcco(text);

    ConversationNccoEx conv = new ConversationNccoEx(conversationId);
    conv.setRecord(Boolean.FALSE);
    conv.setMusicOnHoldUrl(musicOnHoldUrl); // for attendee

    conv.setStartOnEnter(Boolean.FALSE); // for attendee
    //conv.setEndOnExit(Boolean.TRUE); // for moderator

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(talk);
    list.add(conv);

    return list;
  }

  public List<Ncco> nccoListWithPromptRecordName(String text, String eventUrl, String finalText) {

    RecordNcco rec = new RecordNcco();
    rec.setEndOnKey(new Character('#'));
    rec.setBeepStart(Boolean.TRUE);
    rec.setEventUrl(eventUrl);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(new TalkNcco(text));
    list.add(rec);
    list.add(new TalkNcco(finalText));

    return list;
  }

  public List<Ncco> nccoListWithPromptCustomerNameForCallbackTask(String streamUrl, String t1,
      String t2, String t3, String eventUrl) {

    InputNcco input = new InputNcco();
    input.setEventUrl(eventUrl);
    input.setTimeOut(20);
    input.setMaxDigits(1);

    ArrayList<Ncco> list = new ArrayList<Ncco>();
    list.add(new TalkNcco(t1));
    list.add(new StreamNcco(streamUrl));
    list.add(new TalkNcco(t2));
    list.add(new StreamNcco(streamUrl));
    list.add(new TalkNcco(t3));
    list.add(input);

    return list;
  }
}
