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
package com.softavail.commsrouter.eval;

import com.softavail.commsrouter.api.dto.model.RouterObjectRef;
import com.softavail.commsrouter.api.dto.model.skill.AttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.AttributeType;
import com.softavail.commsrouter.api.dto.model.skill.EnumerationAttributeDomainDto;
import com.softavail.commsrouter.api.dto.model.skill.SkillDto;
import com.softavail.commsrouter.api.dto.model.skill.StringAttributeDomainDto;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.api.exception.NotFoundException;
import com.softavail.commsrouter.api.service.CoreSkillService;
import com.softavail.commsrouter.domain.AttributeGroup;
import static com.softavail.commsrouter.eval.ValidationUtils.validateArguments;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import java.util.Iterator;
import java.util.List;
import org.apache.logging.log4j.LogManager;

/**
 *
 * @author vladislav
 */
public class ValidateRsqlVisitor implements RSQLVisitor<Void, AttributeGroup> {

  private static final org.apache.logging.log4j.Logger LOGGER
          = LogManager.getLogger(ValidateRsqlVisitor.class);

  private final CoreSkillService coreSkillService;

  public ValidateRsqlVisitor(CoreSkillService coreSkillService) {
    this.coreSkillService = coreSkillService;
  }

  @Override
  public Void visit(AndNode andNode, AttributeGroup param) {
    Iterator<Node> nodes = andNode.iterator();
    while (nodes.hasNext()) {
      nodes.next().accept(this, null);
    }
    return null;
  }

  @Override
  public Void visit(OrNode orNode, AttributeGroup param) {
    Iterator<Node> nodes = orNode.iterator();
    while (nodes.hasNext()) {
      nodes.next().accept(this, null);
    }
    return null;
  }

  @Override
  public Void visit(ComparisonNode comparisonNode, AttributeGroup param) {
    try {
      validate(comparisonNode.getSelector(), comparisonNode.getOperator(),
              comparisonNode.getArguments());
    } catch (ExpressionException ex) {
      LOGGER.error(ex.getMessage(), ex);
    }
    return null;
  }

  private void validate(String selector, ComparisonOperator operator, List<String> arguments)
      throws ExpressionException {

    // validate existance
    SkillDto skillDto;
    try {
      skillDto = coreSkillService.get(new RouterObjectRef("ref", "routerRef"));
    } catch (NotFoundException ex) {
      throw new ExpressionException("Skill " + selector + " was not found.", ex);
    } catch (CommsRouterException ex) {
      throw new ExpressionException("Error while retrieving skill " + selector + " from database.",
          ex);
    }

    // validate operator restrictions
    validateArguments(operator.getSymbol(), arguments);

    // validate arguments
    AttributeDomainDto attributeDomainDto = skillDto.getDomain();
    if (skillDto.getDomain() != null) {
      AttributeType attributeType = attributeDomainDto.getType();
      switch (attributeType) {
        case bool:
          for (String argument : arguments) {
            ValidationUtils.parseBoolean(argument);
          }
          break;
        case number:
          for (String argument : arguments) {
            ValidationUtils.parseNumber(argument);
          }
          break;
        case string:
          String regExp = ((StringAttributeDomainDto) attributeDomainDto).getRegex();
          for (String argument : arguments) {
            if (!argument.matches(regExp)) {
              throw new ExpressionException(
                  "Value '" + argument + "' not valid for skill " + selector + ".");
            }
          }
          break;
        case enumeration:
          for (String argument : arguments) {
            if (!((EnumerationAttributeDomainDto) attributeDomainDto).getValues()
                .contains(argument)) {
              throw new ExpressionException(
                  "Value '" + argument + "' not valid for skill " + selector + ".");
            }
          }
          break;
      }
    }
  }
}
