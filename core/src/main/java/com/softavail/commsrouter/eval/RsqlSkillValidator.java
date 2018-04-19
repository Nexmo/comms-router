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
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;

import java.util.Iterator;
import java.util.List;

/**
 * @author vladislav
 */
public class RsqlSkillValidator implements RsqlValidator, RSQLVisitor<Void, String> {

  private final CoreSkillService coreSkillService;

  public RsqlSkillValidator(CoreSkillService coreSkillService) {
    this.coreSkillService = coreSkillService;
  }

  @Override
  public Void visit(AndNode andNode, String routerRef) {
    Iterator<Node> nodes = andNode.iterator();
    while (nodes.hasNext()) {
      nodes.next().accept(this, routerRef);
    }
    return null;
  }

  @Override
  public Void visit(OrNode orNode, String routerRef) {
    Iterator<Node> nodes = orNode.iterator();
    while (nodes.hasNext()) {
      nodes.next().accept(this, routerRef);
    }
    return null;
  }

  @Override
  public Void visit(ComparisonNode comparisonNode, String routerRef) {
    try {
      validate(
          comparisonNode.getSelector(),
          comparisonNode.getOperator(),
          comparisonNode.getArguments(),
          routerRef);
    } catch (ExpressionException ex) {
      throw new RuntimeException(ex.getMessage(), ex);
    }
    return null;
  }

  @Override
  public void validate(Node rootNode, String routerRef) throws ExpressionException {
    try {
      rootNode.accept(this, routerRef);
    } catch (RuntimeException ex) {
      throw new ExpressionException(ex.getMessage(), ex);
    }
  }

  private void validate(
      String selector, ComparisonOperator operator, List<String> arguments, String routerRef)
      throws ExpressionException {

    // validate existance
    SkillDto skillDto;
    try {
      skillDto = coreSkillService.get(new RouterObjectRef(selector, routerRef));
    } catch (NotFoundException ex) {
      throw new ExpressionException("Skill " + selector + " was not found.", ex);
    } catch (CommsRouterException ex) {
      throw new ExpressionException("Error while retrieving skill " + selector + " from database.",
          ex);
    }

    // validate arguments
    AttributeDomainDto attributeDomainDto = skillDto.getDomain();
    if (skillDto.getDomain() != null) {
      AttributeType attributeType = attributeDomainDto.getType();
      switch (attributeType) {
        case bool:
          for (String argument : arguments) {
            ValidationUtils.assertBoolean(argument);
          }
          break;
        case number:
          for (String argument : arguments) {
            ValidationUtils.assertNumber(argument);
          }
          break;
        case string:
          String regExp = ((StringAttributeDomainDto) attributeDomainDto).getRegex();
          for (String argument : arguments) {
            if (!argument.matches(regExp)) {
              throw new ExpressionException(
                  argument + "' is not a valid value for skill " + selector + ".");
            }
          }
          break;
        case enumeration:
          for (String argument : arguments) {
            if (!((EnumerationAttributeDomainDto) attributeDomainDto).getValues()
                .contains(argument)) {
              throw new ExpressionException(
                  argument + "' is not a valid value for skill " + selector + ".");
            }
          }
          break;
        default:
          // Nothing to do
      }
    }
  }
}
