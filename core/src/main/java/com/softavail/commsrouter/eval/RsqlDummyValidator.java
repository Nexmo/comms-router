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

import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.OrNode;

/**
 *
 * @author vladislav
 */
public class RsqlDummyValidator implements RsqlValidator {

  @Override
  public Void visit(AndNode node, String routerRef) {
    return null;
  }

  @Override
  public Void visit(OrNode node, String routerRef) {
    return null;
  }

  @Override
  public Void visit(ComparisonNode node, String routerRef) {
    return null;
  }

}
