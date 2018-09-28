package com.softavail.comms.nexmo.util;

public class PhoneConverter {

  public static String machineReadable(String phoneNumber) {

    if (null == phoneNumber) {
      return null;
    }
    
    StringBuilder machineReadable = new StringBuilder(phoneNumber.length() * 2);

    int idx = 0;
    for (char c : phoneNumber.toCharArray()) {
      if (Character.isDigit(c)) {
        if (idx > 0) {
          machineReadable.append(' ');
        }
        
        machineReadable.append(c);
      }
      ++idx;
    }

    return machineReadable.toString();
  }

  public static String normalize(String phoneNumber) {
    
    if (phoneNumber != null && phoneNumber.length() > 9) {
      if (phoneNumber.charAt(0) != '+') {
        return new StringBuilder().append('+').append(phoneNumber).toString();
      } 
    }
    
    return phoneNumber;
  }
}
