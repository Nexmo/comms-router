/*
 * Copyright 2017 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.softavail.commsrouter.api.dto.misc;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Base64;

/**
 * Created by @author mapuo on 04/12/17.
 */
public class PaginationHelper {

  private static final Logger LOGGER = LogManager.getLogger(PaginationHelper.class);
  private static final String DELIMITER = ":";

  public static Long getEntityId(Class classz, String token) {
    if (token != null && !token.trim().isEmpty()) {
      try {
        byte[] bytes = Base64.getDecoder().decode(token);
        String[] split = new String(bytes).split(DELIMITER);
        if (split.length == 2 && classz.getSimpleName().equals(split[0])) {
          return Long.valueOf(split[1]);
        }
      } catch (IllegalArgumentException e) {
        LOGGER.error("Exception while parsing token {}: {}", token, e, e);
      }
    }
    return 0L;
  }

  public static String getToken(Class classz, Long id) {
    byte[] bytes = String.format(
        "%s%c%d", classz.getSimpleName(), DELIMITER.charAt(0), id).getBytes();
    return Base64.getUrlEncoder().encodeToString(bytes);
  }

}
