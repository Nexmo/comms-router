/*
 * Copyright 2017 - 2018 SoftAvail Inc.
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
package com.softavail.commsrouter.api.dto.model.skill;

import com.softavail.commsrouter.api.exception.BadValueException;
import java.util.regex.PatternSyntaxException;
import org.junit.Test;

/**
 *
 * @author ikrustev
 */
public class StringAttributeDomainDtoTest {

  public StringAttributeDomainDtoTest() {
  }

  @Test
  public void testValidateNoRegex() throws BadValueException {
    StringAttributeDomainDto instance = new StringAttributeDomainDto();
    instance.validate();
  }

  @Test
  public void testValidateEmptyRegex() throws BadValueException {
    StringAttributeDomainDto instance = new StringAttributeDomainDto("");
    instance.validate();
  }

  @Test(expected = PatternSyntaxException.class)
  public void testValidateBadRegex() throws BadValueException, Throwable {
    StringAttributeDomainDto instance = new StringAttributeDomainDto("[");
    try {
      instance.validate();
    } catch (BadValueException ex) {
      if (ex.getCause() instanceof PatternSyntaxException) {
        throw ex.getCause();
      }
      throw ex;
    }
  }

  @Test
  public void testValidateRegex() throws BadValueException {
    StringAttributeDomainDto instance = new StringAttributeDomainDto("prefix-[0-9]+");
    instance.validate();
  }

}
