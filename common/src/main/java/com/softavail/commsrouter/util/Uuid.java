package com.softavail.commsrouter.util;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.UUID;

/**
 * Created by @author mapuo on 20.09.17.
 */
public class Uuid {

  private static final char[] ALPHABET = {
      '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
      'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
      'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
      'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
      'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
      'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
      'U', 'V', 'W', 'X', 'Y', 'X'
  };

  private static final BigInteger BASE = BigInteger.valueOf(62);

  private static String toString(BigInteger number) {
    StringBuilder builder = new StringBuilder();
    while (number.compareTo(BigInteger.ZERO) >= 1) {
      BigInteger[] mod = number.divideAndRemainder(BASE);
      builder.append(ALPHABET[mod[1].intValue()]);
      number = mod[0];
    }
    return builder.toString();
  }

  private static BigInteger getRandom() {
    UUID uuid = UUID.randomUUID();
    ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
    bb.putLong(uuid.getMostSignificantBits());
    bb.putLong(uuid.getLeastSignificantBits());
    return new BigInteger(1, bb.array());
  }

  public static String get() {
    return toString(getRandom());
  }

}
