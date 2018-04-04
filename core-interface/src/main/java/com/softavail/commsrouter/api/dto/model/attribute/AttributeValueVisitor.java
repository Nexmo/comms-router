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

package com.softavail.commsrouter.api.dto.model.attribute;

import com.softavail.commsrouter.api.exception.CommsRouterException;

/**
 *
 * @author ikrustev
 */
public interface AttributeValueVisitor {

  void handleBooleanValue(BooleanAttributeValueDto value) throws CommsRouterException;

  void handleDoubleValue(DoubleAttributeValueDto value) throws CommsRouterException;

  void handleStringValue(StringAttributeValueDto value) throws CommsRouterException;

  void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws CommsRouterException;

  void handleArrayOfDoublesValue(ArrayOfDoublesAttributeValueDto value) throws CommsRouterException;

}
