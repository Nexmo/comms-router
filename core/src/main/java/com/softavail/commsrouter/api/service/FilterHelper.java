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

import com.github.tennaito.rsql.jpa.JpaPredicateVisitor;
import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.RSQLVisitor;

import java.util.Optional;
import javax.persistence.EntityManager;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

/**
 * Created by @author mapuo on 15/01/18.
 */
public class FilterHelper {

  public static Optional<Predicate> filterPredicate(String query, Root root, EntityManager em) {
    if (query != null && !query.isEmpty()) {
      // Create the JPA Visitors
      RSQLVisitor<Predicate, EntityManager> visitor = new JpaPredicateVisitor<>().defineRoot(root);

      // Parse a RSQL into a Node
      Node rootNode = new RSQLParser().parse(query);

      // Visit the node to retrieve CriteriaQuery
      Predicate predicate = rootNode.accept(visitor, em);
      return Optional.of(predicate);
    }

    return Optional.empty();
  }

}
