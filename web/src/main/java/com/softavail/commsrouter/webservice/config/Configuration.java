/* 
 * Copyright 2017 SoftAvail Inc.
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

package com.softavail.commsrouter.webservice.config;

/**
 * Created by @author mapuo on 16.10.17.
 */
public interface Configuration {

  String SYSTEM_PROPERTY_KEY = "comms.router.config.file";

  Configuration DEFAULT = new Configuration() {
    @Override
    public Integer getClientConnectTimeout() {
      return 1500;
    }

    @Override
    public Integer getClientReadTimeout() {
      return 1500;
    }

    @Override
    public Boolean getClientFollowRedirects() {
      return true;
    }

    @Override
    public String getShiroConfigLocations() {
      return "classpath:shiro.ini";
    }

  };


  Integer getClientConnectTimeout();

  Integer getClientReadTimeout();

  Boolean getClientFollowRedirects();
  
  String getShiroConfigLocations();

}
