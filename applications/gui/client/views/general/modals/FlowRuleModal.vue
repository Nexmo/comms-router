<template>
  <!--TODO: change to vlt-modal large -->
  <vlt-modal :visible="visible" :title="isEdit ? $t('message.flowsView.editRule') : $t('message.flowsView.addRule')"
             @confirm="ok" @close="close"
             :extra-btn-label="isEdit ? $t('message.flowsView.deleteRule') : null" large
             extra-btn-class="Vlt-btn--destructive" extra-btn-icon="bin" extra-btn-icon-pack="fa" @extraBtnClick="deleteBtn" role="dialog">
    <section>
      <vlt-field :label="$t('message.name')">
        <vlt-input id="name-input" v-model="newRule.tag" :val="newRule.tag" v-focus />
      </vlt-field>

      <label class="label">{{$t('message.flowsView.predicate')}}</label>

      <vlt-field :label="$t('message.raw')">
        <vlt-switch v-model="rawPredicate" :checked="rawPredicate" />
      </vlt-field>

      <vlt-field v-if="rawPredicate" >
        <vlt-input textarea v-model="newRule.predicate" :val="newRule.predicate" maxlength="255" />
      </vlt-field>

      <vue-query-builder
        v-else
        :rules="{}"
        :maxDepth="10"
        :labels="{}"
        :rsqlQuery="newRule.predicate"
        :routerRef="routerRef"
        @rsqlError="onRqslError"
        @queryUpdated="queryUpdated">
      </vue-query-builder>

      <small v-if="errors.rsql" class="Vlt-form__element__error">{{errors.rsql}}</small>
    </section>
  </vlt-modal>
</template>

<script>
  import Vue from 'vue'
  import {getSkills, getSkill} from "../../../api/api-client"
  import config from 'buefy/src/utils/config'
  import BSwitch from "buefy/src/components/switch/Switch";
  import VueQueryBuilder from "../../skills-query-builder/VueQueryBuilder";

  import { VltField, VltInput, VltModal, VltSwitch } from '../../../assets/volta/vue';

  export default {
    components: {
      VltField,
      VltInput,
      VltModal,
      VltSwitch,
      VueQueryBuilder
    },

    props: {
      visible: Boolean,
      title: String,
      rule: Object,
      routerRef: String,
      onConfirm: Function,
      onCancel: Function,
      onDelete: Function
    },

    data() {
      return {
        newRule: {},
        isEdit: false,
        errors: {},
        rawPredicate: false
      }
    },

    mounted() {
      console.log("RULE" + this.rule);
      this.isEdit = this.rule && this.rule.tag;
    },

    watch: {
      rawPredicate: function(val) {
        if (!val) {
          this.$delete(this.errors, 'rsql');
        }
      }
    },

    methods: {
      ok: function () {
        if(this.isEdit) {
          this.$emit('update', this.newRule);
        } else {
          this.$emit('add', this.newRule);
        }
      },

      cancel: function () {
        if (typeof this.onCancel === 'function') {
          this.onCancel();
        }
        this.close();
      },

      close() {
        this.$emit('close');
      },

      deleteBtn: function () {
        if (this.isEdit) {
          this.onDelete();
        }
        this.close();
      },
      queryUpdated: function(newQuery) {
        let predicate = this.newRule.predicate ? this.newRule.predicate.toLowerCase() : '';
        if (!this.errors.rsql && predicate !== 'true' && predicate !== 'false') {
          this.newRule.predicate = newQuery;
        }
      },

      onRqslError: function() {
        let predicate = this.newRule.predicate.toLowerCase();
        if (predicate !== 'true' && predicate !== 'false') {
          this.$set(this.errors, 'rsql', this.$i18n.t('message.errors.failedParsePredicate'));
        }
        this.$nextTick(function () {
          this.rawPredicate = true;
        });
      }

    },

    beforeMount () {
      Object.assign(this.newRule, this.rule);
    },
  }
</script>

<style lang="scss" scoped>
  .modal-card-body {
    padding: 0;
  }

  .modal-card-foot {
    justify-content: flex-end;
  }

  .tag {
    margin: 5px;
  }

  .modal {
    z-index: 1996;
  }

</style>
