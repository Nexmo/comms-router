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

import static com.softavail.commsrouter.eval.ValidationUtils.validateAttributes;

import com.softavail.commsrouter.api.exception.ExpressionException;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Vladislav Todorov
 */
public class EvalRsqlVisitor implements RSQLVisitor<Boolean, AttributeGroup> {

  private static final Logger LOGGER = LogManager.getLogger(EvalRsqlVisitor.class);

  @Override
  public Boolean visit(AndNode andNode, AttributeGroup attributeGroup) {

    Boolean result = true;
    Iterator<Node> nodes = andNode.iterator();

    while (result && nodes.hasNext()) {
      result = result && nodes.next().accept(this, attributeGroup);
    }

    return result;
  }

  @Override
  public Boolean visit(OrNode orNode, AttributeGroup attributeGroup) {

    Boolean result = false;
    Iterator<Node> nodes = orNode.iterator();

    while (!result && nodes.hasNext()) {
      result = result || nodes.next().accept(this, attributeGroup);
    }

    return result;
  }

  @Override
  public Boolean visit(ComparisonNode comparisonNode, AttributeGroup attributeGroup) {

    Boolean result = null;
    try {
      result = compare(comparisonNode.getSelector(), comparisonNode.getOperator(),
          comparisonNode.getArguments(), attributeGroup);
    } catch (ExpressionException ex) {
      throw new RuntimeException(ex.getMessage(), ex);
    }

    return result;
  }

  private Boolean compare(
      String selector,
      ComparisonOperator comparisonOperator,
      List<String> arguments,
      AttributeGroup attributeGroup)
      throws ExpressionException {

    List<Attribute> attributes = attributeGroup.getAttributes(selector);
    String operator = comparisonOperator.getSymbol();

    validateAttributes(operator, attributes);

    if (attributes.isEmpty()) {
      switch (operator) {
        case "==":
        case "=gt=":
        case ">":
        case "=ge=":
        case ">=":
        case "=lt=":
        case "<":
        case "=le=":
        case "<=":
        case "=in=":
          return false;
        case "!=":
        case "=out=":
          return true;
        default:
          throw new ExpressionException("Unsupported operator: " + comparisonOperator.getSymbol());
      }
    }

    Attribute.Type type = attributes.get(0).getType();

    switch (operator) {
      case "==":
        return getValues(attributes).contains(parseArgument(arguments.get(0), type));
      case "!=":
        return !getValues(attributes).contains(parseArgument(arguments.get(0), type));
      case "=gt=":
      case ">":
        return compareType(attributes.get(0), arguments.get(0)) > 0;
      case "=ge=":
      case ">=":
        return compareType(attributes.get(0), arguments.get(0)) >= 0;
      case "=lt=":
      case "<":
        return compareType(attributes.get(0), arguments.get(0)) < 0;
      case "=le=":
      case "<=":
        return compareType(attributes.get(0), arguments.get(0)) <= 0;
      case "=in=":
        return compareIn(attributes, parseArguments(arguments, type));
      case "=out=":
        return !compareIn(attributes, parseArguments(arguments, type));
      default:
        throw new ExpressionException("Unsupported operator: " + comparisonOperator.getSymbol());
    }
  }

  private List<Object> getValues(List<Attribute> attributes) {
    return attributes.stream().map(Attribute::getValue).collect(Collectors.toList());
  }

  private Object parseArgument(String argument, Attribute.Type type) {
    switch (type) {
      case STRING:
        return argument;
      case DOUBLE:
        return Double.parseDouble(argument);
      case BOOLEAN:
        return Boolean.parseBoolean(argument);
      default:
        throw new RuntimeException("Unexpected argument type");
    }
  }

  private List<Object> parseArguments(List<String> arguments, Attribute.Type type) {
    return arguments.stream().sequential().map(argument -> parseArgument(argument, type))
        .collect(Collectors.toList());
  }

  private int compareType(Attribute attribute, String argument) {
    switch (attribute.getType()) {
      case STRING:
        return attribute.getStringValue().compareTo(argument);
      case DOUBLE:
        return attribute.getDoubleValue().compareTo(Double.parseDouble(argument));
      case BOOLEAN:
        return attribute.getBooleanValue().compareTo(Boolean.parseBoolean(argument));
      default:
        throw new RuntimeException("Unexpected attribute type " + attribute.getType() + " for "
            + attribute.getName() + "in " + attribute.getAttributeGroup().getId());
    }
  }

  private boolean compareIn(List<Attribute> attributes, List<Object> arguments) {
    return attributes.stream().anyMatch(attribute -> arguments.contains(attribute.getValue()));
  }

}
