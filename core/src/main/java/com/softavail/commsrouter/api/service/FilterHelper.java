/*
 * Copyright 2018 SoftAvail, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.softavail.commsrouter.api.service;

import com.github.tennaito.rsql.builder.BuilderTools;
import com.github.tennaito.rsql.jpa.JpaPredicateVisitor;
import com.github.tennaito.rsql.jpa.PredicateBuilder;
import com.github.tennaito.rsql.jpa.PredicateBuilderStrategy;
import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import cz.jirutka.rsql.parser.ast.LogicalNode;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.OrNode;
import cz.jirutka.rsql.parser.ast.RSQLOperators;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

/**
 * Created by @author mapuo on 15/01/18.
 */
public class FilterHelper {

  private static final Logger LOGGER = LogManager.getLogger(FilterHelper.class);

  private static final String ATTR_OPERATOR = "=attr=";

  public static Optional<Predicate> filterPredicate(String query, Root root, EntityManager em) {
    if (query != null && !query.isEmpty()) {
      // Create the JPA Visitors
      AttributeOpStrategy predicateStrategy = new AttributeOpStrategy();
      FilterPredicateVisitor<?> jpaVisitor = new FilterPredicateVisitor<>();
      jpaVisitor.getBuilderTools().setPredicateBuilder(predicateStrategy);
      RSQLVisitor<Predicate, EntityManager> visitor = jpaVisitor.defineRoot(root);

      Set<ComparisonOperator> operators = RSQLOperators.defaultOperators();
      operators.add(new ComparisonOperator(ATTR_OPERATOR, true));

      // Parse a RSQL into a Node
      Node rootNode = new RSQLParser(operators).parse(query);

      // Visit the node to retrieve CriteriaQuery
      Predicate predicate = rootNode.accept(visitor, em);
      return Optional.ofNullable(predicate);
    }

    return Optional.empty();
  }

  public static class FilterPredicateVisitor<T> extends JpaPredicateVisitor<T> {

    private From root;

    @Override
    public FilterPredicateVisitor<T> defineRoot(From root) {
      this.root = root;
      super.defineRoot(root);
      return this;
    }

    @Override
    public Predicate visit(AndNode nodes, EntityManager entityManager) {
      CriteriaBuilder builder = entityManager.getCriteriaBuilder();

      List<Predicate> predicates = getPredicates(nodes, entityManager);

      return builder.and(predicates.toArray(new Predicate[0]));
    }

    @Override
    public Predicate visit(OrNode nodes, EntityManager entityManager) {
      CriteriaBuilder builder = entityManager.getCriteriaBuilder();

      List<Predicate> predicates = getPredicates(nodes, entityManager);

      return builder.or(predicates.toArray(new Predicate[0]));
    }

    @Override
    public Predicate visit(ComparisonNode node, EntityManager entityManager) {
      try {
        return getBuilderTools().getPredicateBuilder().createPredicate(
            node, root, entityClass, entityManager, getBuilderTools());
      } catch (IllegalArgumentException exp) {
        return super.visit(node, entityManager);
      }
    }

    private List<Predicate> getPredicates(LogicalNode nodes, EntityManager entityManager) {
      return nodes.getChildren()
          .stream() // .parallelStream()?
          .map(node -> {
            if (node instanceof ComparisonNode) {
              ComparisonNode comparisonNode = (ComparisonNode) node;
              return visit(comparisonNode, entityManager);
            }
            if (node instanceof OrNode) {
              OrNode orNode = (OrNode) node;
              return visit(orNode, entityManager);
            }
            if (node instanceof AndNode) {
              AndNode andNode = (AndNode) node;
              return visit(andNode, entityManager);
            }
            return null;
          })
          .collect(Collectors.toList());
    }

  }

  public static class AttributeOpStrategy implements PredicateBuilderStrategy {

    private static final Logger LOGGER = LogManager.getLogger(AttributeOpStrategy.class);

    private static final Pattern SELECTOR_PATTERN = Pattern.compile(
        "(?<property>[^\\.]+)\\.(?<key>\\w+)");

    @Override
    public <T> Predicate createPredicate(
        Node node,
        From root,
        Class<T> entity,
        EntityManager manager,
        BuilderTools tools)
        throws IllegalArgumentException {

      LOGGER.debug("root: {}", root);
      LOGGER.debug("root: {}", root.getJavaType());

      ComparisonNode comp = (ComparisonNode) node;

      LOGGER.debug("operator: {}", comp.getOperator());
      LOGGER.debug("selector: {}", comp.getSelector());
      LOGGER.debug("arguments: {}", comp.getArguments());

      if (!comp.getOperator().getSymbol().equals(ATTR_OPERATOR)) {
        throw new IllegalArgumentException(
            "AttributeOpStrategy does not support operator: " + comp.getOperator().getSymbol());
      }

      Matcher matcher = SELECTOR_PATTERN.matcher(comp.getSelector());
      if (matcher.find()) {
        String property = matcher.group("property");
        String key = matcher.group("key");
        LOGGER.debug("property: {}, key: {}", property, key);

        CriteriaBuilder cb = manager.getCriteriaBuilder();

        Join attributeGroup = root.join(property);
        Join attribute = attributeGroup.join("attributes");

        Path stringValue = attribute.get("stringValue");
        if (comp.getArguments().size() == 1) {
          String argument = comp.getArguments().get(0);
          String like = argument.replace(PredicateBuilder.LIKE_WILDCARD, '%');
          Predicate where = cb.and(
              cb.equal(attribute.get("name"), key),
              cb.like(stringValue, like));

          return cb.and(where);
        } else {
          Predicate where = cb.and(
              cb.equal(attribute.get("name"), key),
              stringValue.in(comp.getArguments()));

          return cb.and(where);
        }
      }

      return null;
    }

  }

}
