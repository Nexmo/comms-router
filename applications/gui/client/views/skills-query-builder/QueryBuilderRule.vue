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
    <div v-if="typeof rule.inputType !== 'undefined'">
      <div class="Vlt-grid Vlt-grid--narrow">
        <div class="Vlt-col">
          <vlt-dropdown v-if="typeof rule.operands !== 'undefined'" v-model="query.selectedOperator" showSelection :selected="query.selectedOperator" :label="rule.label" :options="rule.operators" />
          <vlt-dropdown v-if="rule.operators" v-model="query.selectedOperator" showSelection :selected="query.selectedOperator" :label="rule.label" :options="rule.operators" />
        </div>

        <div class="Vlt-col">
          <vlt-field v-if="rule.inputType === 'text'" >
            <vlt-input :val="query.value" v-model="query.value" type="text" :placeholder="$t('message.enterString')" />
          </vlt-field>
          <vlt-field v-else-if="rule.inputType === 'number'" >
            <vlt-input :val="query.value" v-model="query.value" type="number" :placeholder="$t('message.enterNumber')"/>
          </vlt-field>

          <vlt-field v-if="rule.inputType === 'select'">
            <vlt-dropdown-native>
              <select v-model="query.value">
                <option v-for="choice in rule.choices" :value="choice.value">{{ choice.label }}</option>
              </select>         
            </vlt-dropdown-native>
          </vlt-field>
        </div>

        <div class="Vlt-col Vlt-col--1of4 Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary Vlt-btn--icon" @click="remove"">
            <vlt-icon icon="bin" />
          </button>
        </div>
      </div>
    </div>

    <div v-else>
      <div class="Vlt-grid Vlt-grid--narrow">
        <div class="Vlt-col">
          <vlt-field>
            <vlt-input disabled :val="`${rule.label} ${query.selectedOperator} ${query.value}`" />
          </vlt-field>
        </div>
        <div class="Vlt-col Vlt-col--1of4 Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary Vlt-btn--icon" @click="remove"">
            <vlt-icon icon="bin" />
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import deepClone from './utilities.js';

import { VltDropdown, VltDropdownNative, VltIcon, VltInput, VltField } from "../../assets/volta/vue";

export default {
  components: {
    VltIcon,
    VltInput,
    VltField,
    VltDropdown,
    VltDropdownNative
  },

  name: "query-builder-rule",

  props: ['query', 'index', 'rule', 'labels'],

  beforeMount () {
    if (this.rule.type === 'custom-component') {
      this.$options.components[this.id] = this.rule.component;
    }
  },

  methods: {
    remove: function() {
      this.$emit('child-deletion-requested', this.index);
    },
    updateQuery(value) {
      let updated_query = deepClone(this.query);
      updated_query.value = value;
      this.$emit('update:query', updated_query);
    }
  },


  mounted () {
    let updated_query = deepClone(this.query);
    if (this.rule.type === 'select') {
      updated_query.value = this.rule.choices[0].value;
      this.$emit('update:query', updated_query);
    }
  }
}
</script>
<style lang="scss" scoped>  
  .Cr-query__rule {
    padding-bottom: 10px;
  }

</style>
