package com.softavail.comms.demo.application.factory;

import com.google.i18n.phonenumbers.NumberParseException;
import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat;
import com.google.i18n.phonenumbers.Phonenumber.PhoneNumber;

import com.nexmo.client.voice.Endpoint;
import com.nexmo.client.voice.PhoneEndpoint;
import com.nexmo.client.voice.SipEndpoint;

public class NexMoModelFactory {

  private static final PhoneNumberUtil PNU = PhoneNumberUtil.getInstance();
  private static final String UNKNOWN_REGION = "ZZ";
  private static final String PLUS_SIGN_PREFIX = "+";
  private static final String EMPTY_STRING = "";

  public static Endpoint createEndpoint(String value) {
    Endpoint endpoint = null;

    if (null != value) {
      if (value.startsWith("sip:")) {
        endpoint = new SipEndpoint(value);
      } else {
        endpoint = new PhoneEndpoint(formatPhone(value));
      }
    }

    return endpoint;
  }

  private static String formatPhone(final String phoneNumber) {
    if (PNU.isPossibleNumber(phoneNumber, UNKNOWN_REGION)) {
      return parsePhone(phoneNumber);
    }

    if (!phoneNumber.startsWith(PLUS_SIGN_PREFIX)) {
      String phoneNumberWithPrefix = PLUS_SIGN_PREFIX + phoneNumber;
      return formatPhone(phoneNumberWithPrefix); // Recursion
    }

    return phoneNumber;
  }

  private static String parsePhone(final String numberToParse) {
    try {
      PhoneNumber phoneNumber = PNU.parseAndKeepRawInput(numberToParse, UNKNOWN_REGION);
      return PNU.format(phoneNumber, PhoneNumberFormat.E164)
          // Strip leading '+' as the Nexmo doesn't expect it
          .replace(PLUS_SIGN_PREFIX, EMPTY_STRING);
    } catch (NumberParseException ignore) {
      // Nothing to do
    }
    return numberToParse;
  }

}
