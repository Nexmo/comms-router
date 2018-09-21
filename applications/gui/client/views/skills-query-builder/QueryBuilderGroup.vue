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
    <div class="Cr-query__group">
      <div class="Vlt-grid">
        <div class="Vlt-col">
         <vlt-field :label="labels.matchType"> 
          <vlt-radio inline :label="labels.matchTypeAll" :val="labels.matchTypeAll" :name="query.logicalOperator" v-model="query.logicalOperator" :checked="query.logicalOperator === labels.matchTypeAll"/>
          <vlt-radio inline :label="labels.matchTypeAny" :val="labels.matchTypeAny" :name="query.logicalOperator" v-model="query.logicalOperator" :checked="query.logicalOperator === labels.matchTypeAny"/>
         </vlt-field>
        </div>
        <div class="Vlt-col Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--tertiary" v-if="this.depth > 1" @click="remove">
            <vlt-icon icon="bin" />
          </button>
        </div>
      </div>
     
      <div class="Cr-query__block" v-for="(child, index) in query.children" :key="index">
        <component
          :is="child.type"
          :type="child.type"
          :query.sync="child.query"
          :rules="rules"
          :rule="ruleById(child.query.rule)"
          :index="index"
          :maxDepth="maxDepth"
          :depth="depth + 1"
          :labels="labels"
          :routerRef="routerRef"
          :addRuleFunc="addRuleFunc"
          v-on:child-deletion-requested="removeChild">
        </component>
      </div>
      
      <div class="Vlt-grid Vlt-grid--middle">
        <div class="Vlt-col">
          <div class="Vlt-form__group">
            <vlt-dropdown 
              v-model="skillName"
              :options="skills"
              property="ref"
              :label="$t('message.skill')"
              @input="loadSkills"
              show-selection
              hide-label
            />

            <button @click="addRule" class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary" :disabled="selectedSkill == null">
              {{labels.addRule}}
            </button>
          </div>
        </div>

        <div class="Vlt-col Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary" v-if="this.depth < this.maxDepth" @click="addGroup" v-html="labels.addGroup"></button>
        </div>
      </div>   
    
    </div>
</template>

<script>
import QueryBuilderRule from './QueryBuilderRule.vue';
import deepClone from './utilities.js';
import _ from 'lodash';
import {getSkills, getSkill} from "../../api/api-client";

import { VltDropdown, VltDropdownNative, VltField, VltIcon, VltRadio } from "../../assets/volta/vue";


export default {
  name: "query-builder-group",

  components: {
    QueryBuilderRule,
    VltDropdown,
    VltDropdownNative,
    VltField,
    VltIcon,
    VltRadio
  },

  props: ['type', 'query', 'rules', 'index', 'maxDepth', 'depth', 'labels', 'routerRef', 'addRuleFunc'],

  data () {
    return {
      skillName: '',
      selectedSkill: null,
      skills: [],
    }
  },

  watch: {
    skillName(selectedSkill) {
      if (!selectedSkill) {
        return;
      }
      this.selectedSkill = selectedSkill;
    }
  },

  methods: {
    ruleById (ruleId) {
      if (!ruleId) {
        return null;
      }
      return this.rules[ruleId];
    },

    addRule () {
      if (this.selectedSkill == null) {
        return;
      }

      if (this.addRuleFunc) {
        this.addRuleFunc(this.selectedSkill);
      } else {
        return;
      }
      let selectedRule = this.ruleById(this.selectedSkill.ref);
      let updated_query = deepClone(this.query);
      let child = {
        type: 'query-builder-rule',
        query: {
          rule: this.selectedSkill.ref,
          selectedOperator: selectedRule.operators[0],
          selectedOperand: this.selectedSkill.ref,
          value: selectedRule.choices && selectedRule.choices.length ? selectedRule.choices[0].value : null,
        }
      };
      // // A bit hacky, but `v-model` on `select` requires an array.
      // if (this.selectedSkill === 'multi-select') {
      //   child.query.value = [];
      // }
      updated_query.children.push(child);
      this.$emit('update:query', updated_query);

      this.selectedSkill = null;
      this.skillName = null;
    },

    addGroup () {
      let updated_query = deepClone(this.query);
      if ( this.depth < this.maxDepth ) {
        updated_query.children.push({
          type: 'query-builder-group',
          query: {
            logicalOperator: "All",
            children: []
          }
        });
        this.$emit('update:query', updated_query);
      }
    },

    remove () {
      this.$emit('child-deletion-requested', this.index);
    },

    removeChild (index) {
      let updated_query = deepClone(this.query);
      updated_query.children.splice(index, 1);
      this.$emit('update:query', updated_query);
    },

    loadSkills: _.debounce(async function () {
      try {
        const data = await getSkills(this.routerRef);
        this.skills = data.data;
      } catch (e) {
      }
    }, 750),

    onSkillSelected: function(option, index) {
      if (!option) {
        return;
      }
      this.selectedSkill = option;
    },
  }
}
</script>
<style lang="scss" scoped>
  @import "~assets/volta/scss/lib/_variables.scss";

  .Cr-query__block {
    border-bottom: 1px solid $grey-light;
  }

  .Cr-query__group .Cr-query__group {
    background: $grey-lighter;   
    padding: 10px;
  }

  .Cr-query__skill {
    align-items: baseline;
  }

  /*.Cr-query__skill__actions {
    align-items: flex-end;   
  }*/
</style>
