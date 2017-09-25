/*
 * To change this license header, choose License Headers in Project Properties. To change this
 * template file, choose Tools | Templates and open the template in the editor.
 */

package com.softavail.commsrouter.api.dto.model.attribute;

import java.io.IOException;

/**
 *
 * @author ikrustev
 */
public interface AttributeValueVisitor {

  void handleBooleanValue(BooleanAttributeValueDto value) throws IOException;

  void handleLongValue(LongAttributeValueDto value) throws IOException;

  void handleStringValue(StringAttributeValueDto value) throws IOException;

  void handleArrayOfStringsValue(ArrayOfStringsAttributeValueDto value) throws IOException;

  void handleArrayOfLongsValue(ArrayOfLongsAttributeValueDto value) throws IOException;

  void handleArrayOfBooleansValue(ArrayOfBooleansAttributeValueDto value) throws IOException;

}
