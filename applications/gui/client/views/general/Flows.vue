<template>
  <section>
    <section v-if="!flow">
      <div class="Vlt-grid">
        <div class="Vlt-col">
           <h1 class="Vlt-title--icon"><vlt-icon icon="flow" class="Vlt-blue"/>{{$t('message.flows')}}</h1>
        </div>
        <div v-if="rows.length === 0" class="Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--secondary" @click="addFlow">
            <vlt-icon icon="plus" />
            <span>{{ $t('message.flowsView.add') }}</span>
          </button>
        </div>
      </div>

      <!--NOTE: for now remove ability to add multiple flows from GUI-->
      <!-- <div class="Vlt-grid">
        <div class="Vlt-col Vlt-col--1of3">
          <cr-search v-model="search" />
        </div>
      </div> -->

      <vlt-table :columns="columns" @click="editFlow" :rows="rows" pagination>
        <template slot="item" slot-scope="slotProps">
          <td>{{ slotProps.item.ref }}</td>
          <td>{{ slotProps.item.description }}</td>
          <td>
            <vlt-badge v-for="tag in slotProps.item.tags" :key="tag" small color="orange">
              {{ tag }}
            </vlt-badge>
          </td>
        </template>
      </vlt-table>
    </section>

    <section v-if="flow">
       <div class="Vlt-grid">
        <div class="Vlt-col">
           <h1 class="Vlt-title--icon"><vlt-icon icon="flow" class="Vlt-blue"/>{{flow.ref}}</h1>
        </div>
        <div class="Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--destructive" v-if="originalFlow.ref" @click="onDelete">
            <vlt-icon icon="bin" />
            <span>{{ $t('message.flowsView.deleteFlow') }}</span>
          </button>
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary" @click="onCancel">
            <vlt-icon icon="cross" />
            {{ $t('message.cancel') }}
          </button>
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--secondary" @click="onSave">
            <vlt-icon icon="save" />
            {{ $t('message.save') }}
          </button>
        </div>
      </div>

      <vlt-field :label="$t('message.flowsView.flowName')">
        <vlt-input id="name"  v-model="flow.ref" :val="flow.ref" :disabled="!!originalFlow.ref"/>
      </vlt-field>

      <vlt-field :label="$t('message.flowsView.description')">
        <vlt-input id="descr"  v-model="flow.description" :val="flow.description" maxlength="255"/>
      </vlt-field>

      <flow-graph :graph="flow"
                  :add-rule="addRule"
                  :edit-rule="editRule"
                  :add-action="addAction"
                  :edit-route="editAction"
                  :edit-default-route="editDefaultRoute"/>

      <flow-rule-modal v-if="rule"
                       :visible="!!rule"
                       :rule="rule"
                       :routerRef="routerId"
                       @close="rule = undefined"
                       @add="createRule"
                       @update="updateRule"
                       @delete="deleteRule" />

      <flow-route-modal v-if="route"
                        :visible="!!route"
                        :defaultRoute="isDefaultRoute"
                        :route="route"
                        :routerRef="routerId"
                        @close="route = undefined"
                        @add="createAction"
                        @updateDefault="updateDefaultRoute"
                        @update="updateAction"
                        @delete="deleteAction" />
    </section>
  </section>
</template>

<script>
  import Vue from 'vue';
  import { getFlows, getFlow, createFlow, updateFlow, deleteFlow } from '../../api/api-client';
  import _ from 'lodash';
  import FlowGraph from "../../components/graph/FlowGraph";
  import FlowRuleModal from "./modals/FlowRuleModal";
  import FlowRouteModal from "./modals/FlowRouteModal";
  import { notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";
  import { confirmDelete } from "../../utils/Utils";

  import { VltBadge, VltField, VltIcon, VltInput, VltTable } from '../../assets/volta/vue';

  export default {
    components: {
      CrSearch,
      FlowGraph,
      FlowRouteModal,
      FlowRuleModal,
      VltBadge,
      VltField,
      VltIcon,
      VltInput,
      VltTable
    },

    data() {
      return {
        routerId: this.$store.state.app.routerRef,
        isDefaultRoute: false,
        isLoading: false,
        perPage: 10,
        sortField: 'ref',
        sortOrder: 'asc',
        sortableFields: ['name', 'description'],
        tokens: [''],
        search: '',
        totalCount: 11,
        currentPage: 1,
        rows: [],
        flow: null,
        originalFlow: null,
        columns: [
          { title: 'Name', property: 'ref', sortable: true },
          { title: 'Description', property: 'description', sortable: true },
          { title: 'Tags' }
        ],
        route: undefined,
        rule: undefined
      }
    },

    methods: {
      onCancel: function () {
        this.flow = null;
        this.$router.push({path: `/${this.routerId}/flows/`});
        this.retrieveFlows();
      },

      onSave: function () {
        if (!this.flow.defaultRoute || !this.flow.defaultRoute.queueRef) {
         this.editDefaultRoute(this.flow.defaultRoute);
         notifyError(this, this.$i18n.t('message.flowsView.errors.emptyQueue'));
         return;
        }

        const that = this;
        let modifiedFlow = Object.assign({}, this.flow);
        if (this.originalFlow.ref && this.originalFlow.ref !== '') {
          delete(modifiedFlow.ref);
          delete(modifiedFlow.routerRef);
          updateFlow(this.originalFlow, modifiedFlow)
          .then(response => {
            this.flow = null;
            this.$router.push({path: `/${this.routerId}/flows/`});
            this.retrieveFlows();
          })
          .catch(e => notifyError(that, e));
        } else {
          delete(modifiedFlow.routerRef);
          createFlow(this.routerId, modifiedFlow)
          .then(response => {
            this.flow = null;
            this.$router.push({path: `/${this.routerId}/flows/`});
            this.retrieveFlows();
          })
          .catch(e => notifyError(that, e));
        }
      },

      onDelete: function () {
        const self = this,
              title = this.$i18n.t('message.flowsView.delete.title'),
              message = this.$i18n.t('message.flowsView.delete.prompt', self.flow);

        confirmDelete(self, title, message, () => {
          deleteFlow(self.routerId, self.flow.ref).then(res => {
              self.flow = null;
              self.$router.push({path: `/flows/`});
              self.retrieveFlows();
          }).catch(e => notifyError(self, e));
        });
      },

      onPageChange: function (page) {
        if (page !== this.currentPage) {
          this.currentPage = page;
          this.retrieveFlows();
        }
      },

      onSort: function (field, order) {
        this.currentPage = 1;
        this.tokens = [''];
        this.sortField = field;
        this.sortOrder = order;
        this.retrieveFlows();
      },

      getSortUri: function (sortOrder, sortField) {
        const shortOrder = sortOrder === 'asc' ? '+' : '-';
        return `${shortOrder}${sortField}`;
      },

      parseSortUri: function (sort) {
        let sortOrder = "asc";
        if (sort.startsWith("-")) {
          sortOrder = "desc";
        }
        let sortField = sort.slice(1, sort.length);
        if (!this.sortableFields.includes(sortField)) {
          return [];
        }
        return [sortOrder, sortField];
      },

      getTagsFromRules: function (rules) {
        let tags = [];
        for (let rule of rules) {
          tags.push(rule.tag);
        }
        return tags;
      },

      retrieveFlows: function () {
        this.isLoading = true;
        const sort = this.getSortUri(this.sortOrder, this.sortField);

        let searchStr = '';
        if (this.search) {
          let searchEsc = this.search.replace("'", "\\'");
          searchStr = `ref=='*${searchEsc}*' or description=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getFlows(this.routerId, token, this.perPage, searchStr, sort)
        .then((response) => {
          this.isLoading = false;
          if (response.data.length === 0 && this.currentPage > 1) {
            this.totalCount--;
            this.currentPage--;
            this.$toast.open(this.$t('message.endOfRecords'));
            return;
          }

          for (let row of response.data) {
            row.tags = this.getTagsFromRules(row.rules);
          }

          this.tokens[this.currentPage] = response.nextToken;
          this.totalCount = (this.currentPage - 1) * this.perPage + response.data.length;
          if (response.data.length === this.perPage) {
            this.totalCount++;
          }

          this.rows = response.data;
        })
        .catch(response => {
          this.isLoading = false;
          notifyError(this, response, this.$i18n.t('messages.flowsView.errors.failedRetrieve'));
        });
      },

      addFlow: function () {
        const flow = {
          "ref": null,
          "routerRef": this.routerId,
          "description": null,
          "rules": [],
          "defaultRoute": {}
        };
        this.flow = flow;
        this.originalFlow = Object.assign({}, flow);
        this.$router.replace({
          path: `/${this.routerId}/flows/${Math.floor((1 + Math.random()) * 0x10000).toString(16)}`,
          query: {create: 'flow'}
        });
      },

      editFlow: function (row) {
        const flow = row;
        this.flow = flow;
        this.originalFlow = Object.assign({}, flow);
        this.$router.push({path: `/${this.routerId}/flows/${flow.ref}`});
      },

      addRule: function (clb) {
        this.rule = {};
        this.addRuleCallback = clb;
      },

      createRule: function(rule) {
        rule.routes = [];
        this.flow.rules.push(rule);

        if(this.addRuleCallback) {
          this.addRuleCallback(rule, this.flow.rules.length - 1);
        }

        this.addRuleCallback = undefined;
        this.rule = undefined;
      },

      deleteRule() {
        this.flow.rules.splice(this.ruleIndex, 1);

        if(this.deleteCallback) {
          this.deleteCallback(this.rule, this.ruleIndex);
        }

        this.deleteCallback = undefined;
        this.rule = undefined;
      },

      editRule: function (rule, idx, delClb) {
        this.rule = rule;
        this.ruleIndex = idx;
        this.deleteCallback = delClb;
      },

      updateRule(edit) {
        Object.assign(this.flow.rules[this.ruleIndex], edit);

        this.deleteCallback = undefined;
        this.rule = undefined;
      },

      addAction: function (rule, ruleIdx, clb) {
        this.route = {};
        this.ruleIndex = ruleIdx;
        this.addActionCallback = clb;
      },

      createAction: function(action) {
        this.flow.rules[this.ruleIndex].routes.push(action);

        if (this.addActionCallback) {
          this.addActionCallback(action, this.flow.rules[this.ruleIndex].routes.length - 1);
        }

        this.route = undefined;
        this.ruleIndex = undefined;
        this.addActionCallback = undefined;
      },

      editAction: function (rule, ruleIdx, route, routeIdx, delClb) {
        this.actionRule = rule;
        this.ruleIndex = ruleIdx;
        this.route = route;
        this.routeIndex = routeIdx;
        this.deleteActionCallback = delClb;
      },

      stopActionEdit: function() {
        this.rule = undefined;
        this.ruleIndex = undefined;
        this.route = undefined;
        this.routeIndex = undefined;
        this.deleteActionCallback = undefined;
      },

      deleteAction: function() {
        this.flow.rules[this.ruleIndex].routes.splice(this.routeIndex, 1);

        if (this.deleteActionCallback) {
          this.deleteActionCallback(this.actionRule, this.ruleIndex, this.route, this.routeIndex);
        }

        this.stopActionEdit();
      },

      updateAction: function(edit) {
        this.$set(this.flow.rules[this.ruleIndex].routes, this.routeIndex, edit);
        this.stopActionEdit();
      },

      editDefaultRoute: function (defaultRoute) {
        this.route = defaultRoute;
        this.isDefaultRoute = true;
      },

      updateDefaultRoute(edit) {
        this.flow.defaultRoute = edit;
        this.route = undefined;
        this.isDefaultRoute = false;
      },

      performSearch: _.debounce(function () {
        this.currentPage = 1;
        this.token = [''];
        this.retrieveFlows();
      }, 500)

    },

    watch: {
      search: function () {
        this.performSearch();
      }
    },

    beforeRouteEnter(to, from, next) {
      if (to.params.ref !== undefined) {
        next(vm => {
          getFlow(vm.routerId, to.params.ref)
          .then(
            flow => {
              vm.flow = flow;
              vm.originalFlow = Object.assign({}, flow);
            },
            error => {
              // Is new flow?
              console.log("error:", error);
            });
        })
      } else {
        next(vm => {
          vm.flow = null
        });
      }
    },

    beforeRouteUpdate(to, from, next) {
      const that = this;
      if (to.params.ref !== undefined) {
        getFlow(that.routerId, to.params.ref)
        .then(
          flow => {
            that.flow = flow;
            that.originalFlow = Object.assign({}, flow);
          },
          error => {
            // Is new flow?
            console.log("error:", error);
          });
        next();
      } else {
        that.flow = null;
        next();
      }
    },

    mounted() {
      if (this.$route.query.hasOwnProperty('sort')) {
        let order = this.parseSortUri(this.$route.query.sort);
        if (order.length === 2) {
          [this.sortOrder, this.sortField] = order;
        }
      }
      this.routerId = this.$route.params.routerRef;
      this.retrieveFlows();
    }
  }
</script>

<style lang="scss" scoped>
</style>
