/*
 * Copyright 2018 jungle.
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
import com.softavail.commsrouter.eval.CommsRouterEvaluator;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLOperators;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.logging.log4j.LogManager;


/**
 *
 * @author jungle
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

        List<Object> attributeValues = attributeGroup.getAttributeValues(selector);

        if (attributeValues.isEmpty()) {
            throw new EvaluatorException("AttributeGroup is empty. selector:" + selector +
                    ", comparisionOperator:" + comparisonOperator + ", arguments:" + arguments);
        }

        Boolean isScalar = attributeGroup.isScalar(selector);

        List<Object> parsedArguments = parseArguments(arguments, attributeValues.get(0));
        switch (comparisonOperator.getSymbol()) {
            case "==":
                if (isScalar) {
                    return attributeValues.get(0).equals(parsedArguments.get(0));
                } else {
                    return attributeValues.contains(parsedArguments.get(0));
                }
            case "!=":
                if (isScalar) {
                    return attributeValues.get(0).equals(parsedArguments.get(0));
                } else {
                    return !attributeValues.contains(parsedArguments.get(0));
                }
            case "=gt=": 
            case ">": 
                return (Double)attributeValues.get(0) > (Double)parsedArguments.get(0);
            case "=ge=": 
            case ">=": 
                return (Double)attributeValues.get(0) >= (Double)parsedArguments.get(0);
            case "=lt=": 
            case "<": 
                return (Double)attributeValues.get(0) < (Double)parsedArguments.get(0);
            case "=le=": 
            case "<=": 
                return (Double)attributeValues.get(0) <= (Double)parsedArguments.get(0);
            case "=in=": 
                return parsedArguments.contains(attributeValues.get(0));
            case "=out=": 
                return !parsedArguments.contains(attributeValues.get(0));
            default:
                throw new EvaluatorException("Unsupported operator: " + comparisonOperator.getSymbol());
        }
    }
    
    private List<Object> parseArguments(List<String> arguments, Object attributeValue) {
        List<Object> parsedArguments = new ArrayList<>();
        if (attributeValue instanceof Double) {
            for (String argument : arguments) {
                parsedArguments.add(Double.parseDouble(argument));
            }
        } else if (attributeValue instanceof Boolean) {
            for (String argument : arguments) {
                parsedArguments.add(Boolean.parseBoolean(argument));
            }
        } else if (attributeValue instanceof String) {
            for (String argument : arguments) {
                parsedArguments.add(argument);
            }
        }
        return parsedArguments;
    }

    private List<Object> getAttributesValues(List<Attribute> attributes) {
        List<Object> attributeValues = new ArrayList<>();
        attributes.stream().forEachOrdered((attribute)->{
            attributeValues.add(attribute.getValue());
        });
        return attributeValues;
    }
    
}
