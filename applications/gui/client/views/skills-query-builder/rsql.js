export function parseRsql(rsql) {
  let query = {
    logicalOperator: null,
    children: [],
  };

  parseRules(rsql, 0, query);
  if (query.logicalOperator == null) {
    query.logicalOperator = 'All';
  }

  return query;
}

function parseRules(rsql, index, query) {
  let innerQuery;
  do {
    index = skipWhitespaces(rsql, index);
    let ch = rsql.charAt(index);
    if (ch === '(') {
      let groupRule = {
        type: 'query-builder-group',
        query: {
          logicalOperator: null,
          children: []
        }
      };
      query.children.push(groupRule);
      index = parseRules(rsql, ++index, groupRule.query);
      index = skipWhitespaces(rsql, index);
      ch = rsql.charAt(index++);
      if (ch !== ')') {
        throwException(index);
      }
    } else {
      index = parseRule(rsql, index, query.children);
    }
    index = skipWhitespaces(rsql, index);
    let value = readLogicalOperator(rsql, index);
    if (value == null) {
      break;
    }
    index = value.index;
    if (query.logicalOperator == null) {
      query.logicalOperator = value.value;
    } else if (query.logicalOperator !== value.value) {
      if (query.logicalOperator === 'Any') {
        if (!innerQuery) {
          innerQuery = {
            type: 'query-builder-group',
            query: {
              logicalOperator: 'All',
              children: []
            }
          };
        }
        innerQuery.query.children.push(query.children[query.children.length - 1]);
        query.children.splice(query.children.length - 1, 1, innerQuery);
        query = innerQuery.query;
      } else if (query.logicalOperator === 'All') {
        query.logicalOperator = 'Any';
        innerQuery = {
          type: 'query-builder-group',
          query: {
            logicalOperator: 'All',
            children: query.children
          }
        };
        query.children = [innerQuery];
        innerQuery = null;
      }
    } else if (innerQuery) {
      innerQuery = null;
    }
    ch = rsql.charAt(index);
  } while (true);
  return index;
}

function readLogicalOperator(rsql, index) {
  let ch = rsql.charAt(index);
  let ch1 = rsql.charAt(index + 1);
  let ch2 = rsql.charAt(index + 2);
  if (ch === ';') {
    return {value: 'All', index: index + 1}
  } else if (ch === ',') {
    return {value: 'Any', index: index + 1}
  } else if ((ch === 'o' || ch === 'O') && (ch1 === 'r' || ch1 === 'R')) {
    return {value: 'Any', index: index + 2}
  } else if ((ch === 'a' || ch === 'A') && (ch1 === 'n' || ch1 === 'N') && (ch2 === 'd' || ch2 === 'D')) {
    return {value: 'All', index: index + 3}
  } else if (ch !== ')' && ch !== '') {
    throwException(index);
  }

  return null;
}

function parseRule(rsql, index, children) {
  let statement = {
    type: "query-builder-rule",
    query: {}
  };
  index = skipWhitespaces(rsql, index);
  index = parseOperand(rsql, index, statement.query);
  index = parseOperator(rsql, index, statement.query);
  index = parseRuleValue(rsql, index, statement.query);

  children.push(statement);

  return index;
}

function parseRuleValue(rsql, index, query) {
  if (rsql.charAt(index) === '(') {
    index++;
    let values = [];
    let ch;
    let value;
    do {
      index = skipWhitespaces(rsql, index);
      value = readRsqlValue(rsql, index);
      index = value.index;
      values.push(value.value);
      index = skipWhitespaces(rsql, index);
      ch = rsql.charAt(index);
    } while (!ch || ch === ',');
    if (ch !== ')') {
      throwException(index);
    }
    query.value = value.values;
    return ++index;
  } else {
    index = skipWhitespaces(rsql, index);
    let value = readRsqlValue(rsql, index);

    query.value = value.value;

    return value.index;
  }
}

function parseOperator(rsql, index, query) {
  index = skipWhitespaces(rsql, index);

  let ch = rsql.charAt(index);
  let ch1 = rsql.charAt(index + 1);
  switch (ch) {
    case '=':
      switch (ch1) {
        case '=':
          query.selectedOperator = '==';
          return index + 2;
        case 'g':
          let op = rsql.substring(index, index + 4).toLowerCase();
          if (op === '=gt=') {
            query.selectedOperator = '=gt=';
            return index + 4;
          } else if (op === '=ge=') {
            query.selectedOperator = '=ge=';
            return index + 4;
          }
          throwException(index);
          return index;
        case 'l':
          op = rsql.substring(index, index + 4).toLowerCase();
          if (op === '=lt=') {
            query.selectedOperator = '=lt=';
            return index + 4;
          } else if (op === '=le=') {
            query.selectedOperator = '=le=';
            return index + 4;
          }
          throwException(index);
          return index;
        case 'i':
          op = rsql.substring(index, index + 4).toLowerCase();
          if (op === '=in=') {
            query.selectedOperator = '=in=';
            return index + 4;
          }
          throwException(index);
          return index;
        case 'o':
          op = rsql.substring(index, index + 5).toLowerCase();
          if (op === '=out=') {
            query.selectedOperator = '=out=';
            return index + 5;
          }
          throwException(index);
          return index;
        default:
          throwException(index);
      }
      break;
    case '!':
      if (ch1 === '=') {
        query.selectedOperator = '!=';
        return index + 2;
      }
      throwException(index);
    case '<':
      if (ch1 === '=') {
        query.selectedOperator = '<=';
        return index + 2;
      }
      query.selectedOperator = '<';
      return index + 1;
    case '>':
      if (ch1 === '=') {
        query.selectedOperator = '>=';
        return index + 2;
      }
      query.selectedOperator = '>';
      return index + 1;
    default:
      throwException(index);
  }

  return index;
}

function throwException(index) {
  throw `Bad RSQL at index ${index}`;
}

function parseOperand(rsql, index, query) {
  let value = readRsqlValue(rsql, index);
  query.selectedOperand = value.value;
  query.rule = value.value;
  return value.index;
}

function skipWhitespaces(rsql, index) {
  while (/\s/.test(rsql.charAt(index))) {
    index++;
  }
  return index;
}

function readRsqlValue(rsql, index) {
  let quote = rsql.charAt(index);
  if (quote === '"' || quote === "'") {
    index++;
  } else {
    quote = null;
  }
  let value = '';
  for (let i = index; i < rsql.length; i++) {
    let ch = rsql.charAt(i);

    if (ch === '\\') {
      value += rsql.charAt(++i);
    } else if (quote && ch === quote) {
      return {value: value, index: i + 1};
    } else if (!quote && (ch === ' ' || ch === ',' || ch === ';' || ch === '=' || ch === '!' || ch === '<' || ch === '>'
        || !ch || ch === ')')) {
      return {value: value, index: i};
    } else {
      value += ch;
    }
  }
  //if EOF
  return {value: value, index: index + value.length};
}
