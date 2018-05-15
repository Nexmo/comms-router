/*
 * Copyright 2018 SoftAvail Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softavail.commsrouter.shiro;

/**
 * @author Ergyun Syuleyman
 */
public class ShiroConstants {

  public static final String AUTH_HTTP_CONTENT_FORMAT = "<!DOCTYPE html>\n"
      + "<html>\n"
      + "<head>\n"
      + "<title>Authentication</title>\n"
      + "<meta charset=\"UTF-8\">\n"
      + "<meta content=\"width=device-width, initial-scale=1.0\" name=\"viewport\">\n"
      + "</head>\n"
      + "<body>\n"
      + "<div></div>\n"
      + "<script>\n"
      + "    var search = location.search;\n" + "    if (window.opener) {\n" + "        "
      + "var message = {};\n" + "        message.type = \"sso-message\";\n" + "        "
      + "        message.token = \"%s\";\n" + "        message.accessToken = \"%s\";\n"
      + "        //check for IE11\n"
      + "        if (!(window.ActiveXObject) && \"ActiveXObject\" in window) {\n"
      + "            window.opener.handleSsoAuthentication(message);\n"
      + "            window.close();\n"
      + "        } else {\n"
      + "            window.opener.postMessage(message, '*');\n"
      + "            window.close();\n"
      + "        }\n"
      + "    }     \n"
      + "</script>\n"
      + "</body>\n"
      + "</html>";

  public static final String NO_AUTH_JWT = "eyJhbGciOiJIUzI1NiJ9.eyIkaW50X3Blcm1zIjpbXSw"
      + "ic3ViIjoiQW5vbnltb3VzIiwibGFzdE5hbWUiOlsiQW5vbnltb3VzIl0sInJvbGVzIjpbIkFub255b"
      + "W91cyJdLCJ1c2VyTmFtZSI6WyJBbm9ueW1vdXMiXSwibm90QmVmb3JlIjp7fSwiZmlyc3ROYW1lIjpb"
      + "IkFub255bW91cyJdLCJyZWFsTmFtZSI6WyJBbm9ueW1vdXMiXSwibmFtZSI6IkFub255bW91cyIsIm5"
      + "vdE9uT3JBZnRlciI6e30sIiRpbnRfcm9sZXMiOltdLCJleHBpcmF0aW9uIjoxOTI0OTA1NjAwMDAwLC"
      + "JpYXQiOjE5MjQ5MDU2MDAsInNlc3Npb25pbmRleCI6Il9vNXphbWRuNTFpcnh5YmJmajZhc2ZtZmx4Y"
      + "3phcWlrdmpkcHEzZnMiLCJlbWFpbCI6ImFub255bW91cyJ9.8pdutP-88LYdOouaEwf3BxQg90jD3"
      + "wipTHgzVYHvkyc";

}
