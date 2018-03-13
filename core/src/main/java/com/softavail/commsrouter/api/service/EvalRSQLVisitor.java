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
package com.softavail.commsrouter.api.service;

import com.softavail.commsrouter.api.exception.EvaluatorException;
import com.softavail.commsrouter.domain.Attribute;
import com.softavail.commsrouter.domain.AttributeGroup;
import com.softavail.commsrouter.domain.dto.mappers.AttributesMapper.JpaAttributeValueType;
import static com.softavail.commsrouter.domain.dto.mappers.AttributesMapper.getJpaAttributeValueType;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.logging.log4j.LogManager;


/**
 *
 * @author Vladislav Todorov
 * @param <Boolean>
 * @param <String>
 */
public class EvalRSQLVisitor implements RSQLVisitor<Boolean, AttributeGroup> {

    private static final org.apache.logging.log4j.Logger LOGGER = LogManager.getLogger(EvalRSQLVisitor.class);

    @Override
    public Boolean visit(AndNode andNode, AttributeGroup attributeGroup) {
        
        System.out.println("\nandNode:" + andNode + "\nattributeGroup\n" + attributeGroup.getAttributes());
        
        Boolean result = true;
        Iterator<Node> nodes = andNode.iterator();
        
        while (result && nodes.hasNext()) {
            result = result && nodes.next().accept(this, attributeGroup);
        }
        
        return result;
    }

    @Override
    public Boolean visit(OrNode orNode, AttributeGroup attributeGroup) {
        
        System.out.println("\norNode:" + orNode + "\nattributeGroup\n" + attributeGroup.getAttributes());
        
        Boolean result = false;
        Iterator<Node> nodes = orNode.iterator();
        
        while (!result && nodes.hasNext()) {
            result = result || nodes.next().accept(this, attributeGroup);
        }
        
        return result;
    }

    @Override
    public Boolean visit(ComparisonNode comparisonNode, AttributeGroup attributeGroup) {
        
        System.out.println("\ncomparisonNode:" + comparisonNode + "\nattributeGroup:" + attributeGroup.getAttributes());
        System.out.println("selector:" + comparisonNode.getSelector() + ", operator:" + comparisonNode.getOperator() + ", arguments: " + comparisonNode.getArguments());

        Boolean result = null;
        try {
            result = comapre(comparisonNode.getSelector(), comparisonNode.getOperator(), comparisonNode.getArguments(), attributeGroup);
        } catch (EvaluatorException ex) {
            LOGGER.error(ex.getMessage(), ex);
        }

        System.out.println("result = " + result);
        
        return result;

    }
    
    private Boolean comapre(String selector, ComparisonOperator comparisonOperator, List<String> arguments, AttributeGroup attributeGroup) throws EvaluatorException {

        List<Attribute> attributes = attributeGroup.getAttributes(selector);
        List<Object>    attributeValues = attributes.stream().map(a -> a.getValue()).collect(Collectors.toList());

        validateArguments(comparisonOperator.getSymbol(), arguments);
        validateAttributes(comparisonOperator.getSymbol(), attributes);

        if (attributes.isEmpty()) {
            throw new EvaluatorException("AttributeGroup is empty. Undefined attribute:" + selector +
                    ", comparisionOperator:" + comparisonOperator + ", arguments:" + arguments);
        }

        JpaAttributeValueType type = getJpaAttributeValueType(attributes.get(0));
        List<Object> parsedArguments = parseArguments(arguments, type);

        switch (comparisonOperator.getSymbol()) {
            case "==":
                return attributeValues.contains(parsedArguments.get(0));
            case "!=":
                return !attributeValues.contains(parsedArguments.get(0));
            case "=gt=": 
            case ">":
                return compare(attributes.get(0), arguments.get(0), type) > 0;
            case "=ge=": 
            case ">=":
                return compare(attributes.get(0), arguments.get(0), type) >= 0;
            case "=lt=": 
            case "<": 
                return compare(attributes.get(0), arguments.get(0), type) < 0;
            case "=le=": 
            case "<=": 
                return compare(attributes.get(0), arguments.get(0), type) <= 0;
            case "=in=": 
                return parsedArguments.contains(attributeValues.get(0));
            case "=out=": 
                return !parsedArguments.contains(attributeValues.get(0));
            default:
                throw new EvaluatorException("Unsupported operator: " + comparisonOperator.getSymbol());
        }
    }

    private List<Object> parseArguments(List<String> arguments, JpaAttributeValueType type) {

        List<Object> parsedArguments = new ArrayList<>();
        switch (type) {
            case STRING:
                arguments.forEach((argument) -> {parsedArguments.add(argument);});
                break;
            case DOUBLE:
                arguments.forEach((argument) -> {parsedArguments.add(Double.parseDouble(argument));});
                break;
            case BOOLEAN:
                arguments.forEach((argument) -> {parsedArguments.add(Boolean.parseBoolean(argument));});
                break;
        }
        return parsedArguments;
    }

    private int compare(Attribute attribute, String argument, JpaAttributeValueType type) {
        switch (type) {
            case STRING:
                return attribute.getStringValue().compareTo(argument);
            case DOUBLE:
                return attribute.getDoubleValue().compareTo(Double.parseDouble(argument));
            case BOOLEAN:
                return attribute.getBooleanValue().compareTo(Boolean.parseBoolean(argument));
            default:
                throw new RuntimeException("Unexpected attribute value type " + type + " for " + attribute.getName()
                  + "in " + attribute.getAttributeGroup().getId());
        }
    }

    private void validateArguments(String operator, List<String> arguments) throws EvaluatorException {
        switch (operator) {
            case "==":
            case "!=":
            case "=gt=":
            case ">":
            case "=ge=":
            case ">=":
            case "=lt=":
            case "<":
            case "=le=":
            case "<=":
                if (arguments.size() != 1) {
                    throw new EvaluatorException("Invalid arguments number. Expected 1 but found " + arguments.size());
                }
        }
    }

    private void validateAttributes(String operator, List<Attribute> attributes) throws EvaluatorException {
        switch (operator) {
            case "!=":
            case "=gt=":
            case ">":
            case "=ge=":
            case ">=":
            case "=lt=":
            case "<":
            case "=le=":
            case "<=":
            case "=in=":
            case "=out=":
                if (attributes.size() != 1) {
                    throw new EvaluatorException("Invalid attributes number. Expected 1 but found " + attributes.size());
                }
        }
    }
}
