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

package com.softavail.commsrouter.webservice.resources;

import io.swagger.annotations.Info;
import io.swagger.annotations.License;
import io.swagger.annotations.SwaggerDefinition;

@SwaggerDefinition(
    info = @Info(
        title = "Comms Router Web API",
        description = "RESTful interface to the Comms Router Core Library "
            + "exposes the router functionality as a microservice via a REST API.\n"
            + "The Comms Router Core implements the Nexmo Task Router’s business logic." 
            + "It provides Java interfaces allowing it to be used as a part of a Java project."
            + "It uses interfaces provided by the DB Layer for persistence and queueing.\n"
            + "The Router Core is packaged as a standalone module.",
        version = "0.9",
        license = @License(
           name = "Open Source Initiative", 
           url = "https://opensource.org/licenses")),
    schemes = {SwaggerDefinition.Scheme.HTTP})
public interface CommsRouterApiConfig {

}
