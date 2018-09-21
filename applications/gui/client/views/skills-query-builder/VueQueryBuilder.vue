<!--
  The MIT License (MIT)

  Copyright (c) 2016 Daniel Abernathy

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
-->

<template>
  <div>
    <div v-if="isFetching" class="loading is-loading is-large"></div>
    <query-builder-group
      v-else
      :index="0"
      :query.sync="query"
      :rules="rules"
      :maxDepth="maxDepth"
      :depth="depth"
      :labels="mergedLabels"
      :routerRef="routerRef"
      :addRuleFunc="addRule"
      type="query-builder-group"
      ></query-builder-group>
  </div>
</template>

<script>
import QueryBuilderGroup from './QueryBuilderGroup.vue';
import deepClone from './utilities.js';
import { parseRsql } from "./rsql";
import { getSkill } from "../../api/api-client";

export default {

  components: {
    QueryBuilderGroup
  },
  
  props: {
    // rules: Object,
    labels: {
      type: Object,
      default () {
        return this.defaultLabels;
      }
    },
    maxDepth: {
      type: Number,
      default: 10,
      validator: function (value) {
        return value >= 1
      }
    },
    initialQuery: Object,
    rsqlQuery: String,
    routerRef: String,
  },

  data () {
    return {
      rules: {},
      isFetching: 0,
      depth: 1,
      query: {
        logicalOperator: "All",
        children: []
      },
      defaultLabels: {
        matchType: this.$i18n.t('message.matchType'),
        matchTypeAll: "All",
        matchTypeAny: "Any",
        addRule: this.$i18n.t('message.skillsView.add'),
        removeRule: "&times;",
        addGroup: this.$i18n.t('message.addGroup'),
        removeGroup: "&times;",
        textInputPlaceholder: this.$i18n.t('message.enterSkill'),
      }
    }
  },

  computed: {
    mergedLabels () {
      return Object.assign({}, this.defaultLabels, this.labels);
    },
  },

  mounted () {
    if (this.$options.propsData.rsqlQuery) {
      this.query = this.parseRsqlQuery(this.$options.propsData.rsqlQuery);
    }

    this.$emit('queryUpdated', this.composeRsql((this.query)) );

    this.$watch(
      'query',
      function( newQuery ) {
        let rsql = this.composeRsql(newQuery);
        this.$emit('queryUpdated', rsql );
      }, {
      deep: true
    });

    if ( typeof this.$options.propsData.initialQuery !== "undefined" ) {
      this.query = deepClone(this.$options.propsData.initialQuery);
    }
  },

  methods: {
    parseRsqlQuery: function(rsqlQuery) {
      let query;
      let skills = [];
      try {
        query = parseRsql(rsqlQuery);

        skills = this.getSkillsFromQuery(query);

        for (let skill in skills) {
          this.isFetching++;
          this.loadSkill(skill);
        }
      } catch (e) {
        this.$emit('rsqlError');
        query = {
          logicalOperator: "All",
          children: []
        };
      }

      return query;
    },

    getSkillsFromQuery: function(query) {
      let skillRefs = {};
      for (let rule of query.children) {
        if (rule.type === 'query-builder-rule') {
          skillRefs[rule.query.rule] = null;
        } else if (rule.type === 'query-builder-group') {
          Object.assign(skillRefs, this.getSkillsFromQuery(rule.query));
        }
      }

      return skillRefs;
    },

    loadSkill: async function(skillRef) {
      let skill;
      try {
        skill = await getSkill(this.routerRef, skillRef);
      } catch (e) {}
      if (!skill || !skill.ref) {
        skill = { ref: skillRef };
      }
      this.addRule(skill);
      this.isFetching--;
    },

    addRule: function(skill) {
      // skill.ref = skill.ref.toLowerCase();
      let selectedRule = this.rules[skill.ref];
      if (!selectedRule) {
        selectedRule = {
          id: skill.ref,
          choices: [],
          label: skill.ref,
          // type: 'multi-select' ???
        };
        this.rules[skill.ref] = selectedRule;
      }

      if (!skill.domain) {
        return;
      }

      switch (skill.domain.type) {
        case 'bool':
          selectedRule.inputType = "select";
          selectedRule.operators = ['==', '!='];
          selectedRule.choices = [
            {label: this.$i18n.t('message.true'), value: true},
            {label: this.$i18n.t('message.false'), value: false}
          ];
          break;
        case 'string':
          selectedRule.inputType = "text";
          selectedRule.operators = ['==', '!='];
          break;
        case 'enumeration':
          selectedRule.inputType = "select";
          selectedRule.operators = ['==', '!='];
          selectedRule.choices = [];
          skill.domain.values.forEach((value) => selectedRule.choices.push({label: value, value: value}));
          break;
        case 'number':
          selectedRule.inputType = "number";
          selectedRule.operators = ['==', '!=', '<', '<=', '>', '>='];
          break;
      }
    },

    composeRsql: function(query) {
      let rsql = '';

      const logicalOperator = query.logicalOperator === 'Any' ? ' or ' : ' and ';
      let value;
      for (let statement of query.children) {
        if (statement.type === 'query-builder-rule') {
          if (rsql) {
            rsql += logicalOperator;
          }

          value = this.toRsqlValue(statement.query.value);
          value = Array.isArray(value) ? `(${value.join(',')})` : value;

          rsql += `${this.toRsqlValue(statement.query.selectedOperand)}${statement.query.selectedOperator}${value}`;
        } else if (statement.type === 'query-builder-group') {
          let inner = this.composeRsql(statement.query);
          if (inner) {
            if (rsql) {
              rsql += logicalOperator;
            }

            rsql += `(${inner})`;
          }

        }
      }
      return rsql;
    },

    toRsqlValue: function (value) {
      if (Array.isArray(value)) {
        for (let i in value) {
          value[i] = this.toRsqlValue(value[i]);
        }
        return;
      }
      if (/["'();,=!~<> ]/.test(value)) {
        return `"${value.split('"').join('\\"')}"`
      }

      return value;
    }
  }
}
</script>

<style lang="scss" scoped>
</style>
