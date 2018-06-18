/*
 * Copyright 2017 SoftAvail, Inc.
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

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

import com.softavail.commsrouter.api.dto.misc.PagingRequest;
import com.softavail.commsrouter.api.exception.CommsRouterException;
import com.softavail.commsrouter.domain.ApiObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.reflect.InvocationTargetException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.Attribute;

/**
 * Created by @author mapuo on 04/12/17.
 */
public class PaginationHelper {

  private static final Logger LOGGER = LogManager.getLogger(PaginationHelper.class);
  private static final String DELIMITER = ":";
  private static final String ENTITY_ID_ATTR = "id";
  // className:10,tag:TestTask#0000009,
  private static final String TOKEN_REGEX = "(?:\\s*(?:(?<key>[^:]+):(?<value>[^,]+))\\s*,?)+?";
  private static final Pattern TOKEN_PATTERN = Pattern.compile(TOKEN_REGEX);
  private static final String TOKEN_KEY = "key";
  private static final String TOKEN_VALUE = "value";

  public static final String SORT_REGEX = "(?:\\s*(?:(?<order>[-+])(?<field>[^,]+))\\s*,?){1,3}?";
  private static final String SORT_ORDER = "order";
  private static final String SORT_FIELD = "field";
  private static final Pattern SORT_PATTERN = Pattern.compile(SORT_REGEX);
  private static final Set<String> NON_SORTABLE_FIELDS = ImmutableSet.of("id", "version");

  public static <T> List<Predicate> getSortPredicates(
      CriteriaBuilder cb, Root<T> root, Class classz, PagingRequest request) {

    SeekToken seekToken = SeekToken.parseToken(classz.getSimpleName(), request.getToken());
    Map<String, OrderType> sortOrder = parseSortOrder(request.getSort());

    List<Predicate> predicates = seekToken.getSortParameters().entrySet().stream()
        .map(entry -> {
          String key = entry.getKey();
          String value = entry.getValue();

          Predicate predicate;
          if (sortOrder.get(key) == OrderType.DESCENDING) {
            if (root.get(key).getJavaType().equals(Date.class)) {
              Timestamp timestamp = Timestamp.valueOf(value);
              predicate = cb.lessThan(root.get(key), timestamp);
            } else {
              predicate = cb.lessThan(root.get(key), value);
            }
          } else {
            predicate = cb.greaterThan(root.get(key), value);
          }
          return predicate;
        })
        .collect(
            Builder<Predicate>::new,
            Builder<Predicate>::add,
            (b1, b2) -> b1.addAll(b2.build())).build();

    LOGGER.debug("predicates: {}", predicates);

    return predicates;
  }

  public static <T extends ApiObject> String getToken(T entity, String sort) {
    Class<? extends ApiObject> entityClass = entity.getClass();
    String head = String
        .format("%s%c%d", entityClass.getSimpleName(), DELIMITER.charAt(0), entity.getId());

    Map<String, OrderType> sortOrder = parseSortOrder(sort);

    String collect = sortOrder.keySet().stream()
        .map(key -> {
          Object get = Arrays.stream(entityClass.getMethods())
              .filter(method -> method.getName().startsWith("get"))
              .filter(method -> method.getName().substring(3).equalsIgnoreCase(key))
              .findFirst()
              .map(method -> {
                try {
                  return method.invoke(entity, (Object[]) null);
                } catch (IllegalAccessException | InvocationTargetException e) {
                  LOGGER.error("Can't read field '{}'", key, e);
                }
                return null;
              })
              .orElse(null);

          return key + DELIMITER + get;
        })
        .collect(Collectors.joining(","));

    LOGGER.debug("collect: {}", collect);

    ArrayList<String> strings = Lists.newArrayList(head);
    strings.add(collect);

    String join = String.join(",", strings);
    LOGGER.debug("join: {}", join);

    byte[] bytes = join.getBytes();
    return Base64.getUrlEncoder().encodeToString(bytes).replaceAll("=", "");
  }

  public static <T> List<Order> getSortOrder(CriteriaBuilder cb, Root<T> root, String sort)
      throws CommsRouterException {

    if (sort != null && !sort.isEmpty()) {
      Set<String> attributes = root.getModel().getAttributes().stream()
          .map(Attribute::getName)
          .collect(Collectors.toSet());

      LOGGER.debug("attributes: {}", attributes);

      Map<String, OrderType> sortOrder = parseSortOrder(sort);
      String invalidFields = sortOrder.entrySet().stream()
          .map(Entry::getKey)
          .filter(o -> !attributes.contains(o))
          .collect(Collectors.joining(","));

      if (invalidFields != null && !invalidFields.isEmpty()) {
        throw new CommsRouterException(
            "Field(s) " + invalidFields + " does not exists for this resource");
      }

      Builder<Order> builder = ImmutableList.builder();
      sortOrder.entrySet().stream()
          .filter(entry -> isSortableField(entry.getKey()))
          .forEachOrdered(entry -> {
            Path<Object> path = root.get(entry.getKey());
            Order order;
            if (entry.getValue() == OrderType.DESCENDING) {
              order = cb.desc(path);
            } else {
              order = cb.asc(path);
            }
            builder.add(order);
          });
      builder.add(cb.asc(root.get(ENTITY_ID_ATTR)));

      return builder.build();
    }

    return ImmutableList.of(cb.asc(root.get("id")));
  }

  private static boolean isSortableField(String field) {
    if (NON_SORTABLE_FIELDS.contains(field)) {
      LOGGER.warn("Ignoring sort by provided field: '{}'", field);
      return false;
    }
    return true;
  }

  private static Map<String, OrderType> parseSortOrder(String sort) {
    if (sort != null && !sort.isEmpty()) {

      ImmutableMap.Builder<String, OrderType> builder = ImmutableMap.builder();
      Matcher matcher = PaginationHelper.SORT_PATTERN.matcher(sort);
      while (matcher.find()) {
        String order = matcher.group(PaginationHelper.SORT_ORDER);
        String field = matcher.group(PaginationHelper.SORT_FIELD);
        builder.put(field, OrderType.fromSymbol(order));
      }

      return builder.build();
    }

    return Collections.emptyMap();
  }

  private enum OrderType {
    ASCENDING("+"),
    DESCENDING("-");

    private final String symbol;

    OrderType(String symbol) {
      this.symbol = symbol;
    }

    static OrderType fromSymbol(String symbol) {
      return Arrays.stream(values())
          .filter(orderType -> orderType.symbol.equals(symbol))
          .findFirst()
          .orElse(null);
    }
  }

  private static class SeekToken {

    private final String entityClassName;
    private final Map<String, String> sortParameters;

    private SeekToken(String entityClassName, Map<String, String> sortParameters) {
      this.entityClassName = entityClassName;
      this.sortParameters = sortParameters;
    }

    private static SeekToken parseToken(String className, String token) {
      ImmutableMap.Builder<String, String> identityBuilder = ImmutableMap.builder();
      ImmutableMap.Builder<String, String> paramsBuilder = ImmutableMap.builder();

      decodeToken(token).ifPresent(decodedToken -> {
        Matcher matcher = TOKEN_PATTERN.matcher(decodedToken);
        while (matcher.find()) {
          String key = matcher.group(TOKEN_KEY);
          String value = matcher.group(TOKEN_VALUE);
          if (key.equals(className)) {
            key = ENTITY_ID_ATTR;
            identityBuilder.put(key, value);
            continue;
          }
          paramsBuilder.put(key, value);
        }
      });

      ImmutableMap<String, String> parameters = paramsBuilder.build();

      // If there is no sorting for any param then sort it by id
      if (parameters.size() == 0) {
        parameters = identityBuilder.build();
      }

      return new SeekToken(className, parameters);
    }

    private static Optional<String> decodeToken(String token) {
      return Optional.ofNullable(token)
          .map(t -> {
            try {
              byte[] bytes = Base64.getDecoder().decode(token);
              return new String(bytes);
            } catch (IllegalArgumentException e) {
              LOGGER.error("Exception while parsing token {}: {}", token, e, e);
            }
            return null;
          });
    }

    public String getEntityClassName() {
      return entityClassName;
    }

    public Map<String, String> getSortParameters() {
      return sortParameters;
    }

  }

}
