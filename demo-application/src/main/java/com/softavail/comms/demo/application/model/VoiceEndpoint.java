package com.softavail.comms.demo.application.model;

import com.google.i18n.phonenumbers.NumberParseException;
import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat;
import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.PhoneEndpoint;
import com.nexmo.client.voice.SipEndpoint;

/**
 * Created by @author mapuo on 29.08.17.
 */
public class VoiceEndpoint implements Endpoint {

  private static final PhoneNumberUtil PNU = PhoneNumberUtil.getInstance();
  private static final String UNKNOWN_REGION = "ZZ";

  private Endpoint endpoint;

  public VoiceEndpoint(String endpoint) {
    if (endpoint.startsWith("sip:")) {
      this.endpoint = new SipEndpoint(endpoint);
    } else {
      this.endpoint = new PhoneEndpoint(formatPhone(endpoint));
    }
  }

  private String formatPhone(final String phoneNumber) {
    if (PNU.isPossibleNumber(phoneNumber, UNKNOWN_REGION)) {
      return parsePhone(phoneNumber);
    }

    if (!phoneNumber.startsWith("+")) {
      String phoneNumberWithPrefix = "+" + phoneNumber;
      return formatPhone(phoneNumberWithPrefix); // Recursion
    }

    return phoneNumber;
  }

  private String parsePhone(final String numberToParse) {
    try {
      PhoneNumber phoneNumber = PNU.parseAndKeepRawInput(numberToParse, UNKNOWN_REGION);
      return PNU.format(phoneNumber, PhoneNumberFormat.E164);
    } catch (NumberParseException ignore) {
      // Nothing to do
    }
    return numberToParse;
  }

  @Override
  public String getType() {
    return endpoint.getType();
  }

  @Override
  public String toLog() {
    return endpoint.toLog();
  }

  @Override
  public String toString() {
    return "VoiceEndpoint{"
        + "type=" + endpoint.getType()
        + ", log=" + endpoint.toLog()
        + '}';
  }

}

