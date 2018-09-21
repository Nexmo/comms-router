<template>
  <div class="jsPlumbContainer">
    <div class="Cr-rules">
      <div class="Vlt-grid">
        <div class="Vlt-col Vlt-col--3of4">
          <h3>
            {{ $t('message.flowsView.graph.ruleSetLabel') }}
          </h3>
        </div>
        <div class="Vlt-col Vlt-col--1of4 Vlt-col--right">
          <button class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--tertiary" @click="onAddRule" :aria-label="$t('message.flowsView.addRule')">       
            <vlt-icon icon="plus" small/>
          </button>
        </div>
      </div> 

      <div ref="ruleSet" class="group" :style="`height:${(graph.rules.length + 2) * 190}px`">
        <div v-for="(rule, idx) in graph.rules" :ref="ruleRefId()" class="Cr-rules__item Cr-rules__item--rule" :style="`top:${idx * 175}px;`">
          <div class="Cr-rules__item__title">
            <div class="Vlt-grid">
              <div class="Vlt-col Cr-rules__item__title--truncated">
                <label>{{ rule.tag }}</label>
              </div>
              <div class="Vlt-col Vlt-col--right">
                <span v-if="idx > 0" v-on:click="moveRuleUp(idx)"
                      :data-label="$t('message.flowsView.graph.tooltip.moveRuleUp')">
                  <vlt-icon icon="chevron-up" small class="Vlt-white"/>
                </span>
                <span v-if="idx < graph.rules.length - 1" v-on:click="moveRuleDown(idx)"
                      :data-label="$t('message.flowsView.graph.tooltip.moveRuleDown')">
                  <vlt-icon icon="chevron-down" small class="Vlt-white"/>
                </span>
                <span v-on:click="onEditRule(rule, idx)"
                      :data-label="$t('message.flowsView.graph.tooltip.editRule', rule)">
                  <vlt-icon icon="edit" small class="Vlt-white"/>
                </span>
              </div>
            </div>
          </div>
          <div class="Cr-rules__item__desc">
            <vlt-badge color="green">{{ rule.predicate }}</vlt-badge>
          </div>
        </div>
        <!-- Default Rule -->
        <div ref="defaultRule" class="Cr-rules__item Cr-rules__item--rule"
             :style="`top:${(graph.rules.length) * 175}px;`">
          <span class="Cr-rules__item__title">{{ $t('message.flowsView.graph.defaultRule') }}</span>
          <p class="Cr-rules__item__desc">{{ $t('message.flowsView.graph.defaultRuleDescription') }}</p>      
        </div>
      </div>
    </div>

    <!-- Default Route -->
    <div ref="defaultRoute" class="Cr-rules__item"
         :style="`top:${(graph.rules.length) * 175 + 66}px;left:330px`">
      <div class="Cr-rules__item__title">
        <div class="Vlt-grid">
          <div class="Vlt-col">
            <span>{{ $t('message.flowsView.graph.defaultRoute') }}</span>
          </div>
          <div class="Vlt-col Vlt-col--right Vlt-col--1of4">
          <span v-on:click="editDefaultRoute(graph.defaultRoute)"
                :data-label="$t('message.flowsView.graph.tooltip.editAction')">
            <vlt-icon icon="edit" small class="Vlt-white"/>
          </span>
          </div>
        </div>
      </div>
      <p class="Cr-rules__item__desc">
        <b>{{ $t('message.flowsView.graph.actionType') }}</b> {{ $t(`message.flowsView.graph.actionTypes.routeToQueue`) }}
      </p>
      <p class="Cr-rules__item__desc">
        <b>{{ $t('message.flowsView.graph.priority') }}</b>
        <span place="priority">
          <slot v-if="graph.defaultRoute.priority && graph.defaultRoute.priority > 0">
            {{ graph.defaultRoute.priority }}
          </slot>
          <slot v-else>
            {{ $t('message.flowsView.graph.notSet') }}
          </slot>
        </span>
      </p>
      <p class="Cr-rules__item__desc">
        <b>{{ $t('message.flowsView.graph.timeout') }}</b>
        <span place="timeout">
          <slot v-if="graph.defaultRoute.timeout && graph.defaultRoute.timeout > 0">
            {{ graph.defaultRoute.timeout }} {{ $t('message.flowsView.graph.seconds') }}
          </slot>
          <slot v-else>
            {{ $t('message.flowsView.graph.notSet') }}
          </slot>
        </span>
      </p>
    </div>

    <!-- Default Queue -->
    <div ref="defaultQueue" class="Cr-rules__item Cr-rules__item--queue"
         :style="`top:${(graph.rules.length) * 175 + 76}px;left:620px`">
      <div>
        <vlt-icon icon="stack" class="Vlt-grey" />
        <p class="Cr-rules__item__desc">
          <b>{{ $t('message.flowsView.graph.queue')}}</b>
        </p>
        <p class="Cr-rules__item__desc" place="queueRef">
          <slot v-if="graph.defaultRoute.queueRef">{{ graph.defaultRoute.queueRef  }}</slot>
          <slot v-else>{{ $t('message.flowsView.graph.notSet') }}</slot>
        </p>
      </div>
    </div>

    <slot v-for="(rule, ruleIdx) in graph.rules">
      <!-- Actions -->
      <div v-for="(route, routeIdx) in rule.routes" class="Cr-rules__item" :ref="actionRefId(ruleIdx)"
           :style="`top:${ruleIdx * 175 + 66}px;left:${routeIdx * (250 + 200) + 330}px`">
        <div class="Cr-rules__item__title">
          <div class="Vlt-grid">
            <div class="Vlt-col">
              <span class="title-label">{{ $t('message.flowsView.graph.actionLabel') }}</span>
            </div>
            <div class="Vlt-col Vlt-col--right">     
                <span v-if="routeIdx > 0" v-on:click="moveActionLeft(ruleIdx, routeIdx)"
                      :data-label="$t('message.flowsView.graph.tooltip.moveActionLeft')">
                  <vlt-icon icon="chevron-left" small class="Vlt-white"/>
                </span>
                <span v-if="routeIdx < rule.routes.length - 1"
                      v-on:click="moveActionRight(ruleIdx, routeIdx)"
                      :data-label="$t('message.flowsView.graph.tooltip.moveActionRight')">
                  <vlt-icon icon="chevron-right" small class="Vlt-white"/>
                </span>
                <span v-on:click="onEditAction(rule, ruleIdx, route, routeIdx)"
                      :data-label="$t('message.flowsView.graph.tooltip.editAction')">
                  <vlt-icon icon="edit" small class="Vlt-white"/>
                </span>
            </div>
          </div>
        </div>

        <p class="Cr-rules__item__desc">
          <b>{{ $t('message.flowsView.graph.actionType') }}</b> {{ $t(`message.flowsView.graph.actionTypes.routeToQueue`) }}
        </p>
        <p class="Cr-rules__item__desc">
          <b>{{ $t('message.flowsView.graph.priority') }}</b>
          <span place="priority">
            <slot v-if="graph.defaultRoute.priority && graph.defaultRoute.priority > 0">
              {{ route.priority }}
            </slot>
            <slot v-else>
              {{ $t('message.flowsView.graph.notSet') }}
            </slot>
          </span>
        </p>
        <p class="Cr-rules__item__desc">
          <b>{{ $t('message.flowsView.graph.timeout') }}</b>
          <span place="timeout">
            <slot v-if="graph.defaultRoute.timeout && graph.defaultRoute.timeout > 0">
              {{ route.timeout }} {{ $t('message.flowsView.graph.seconds') }}
            </slot>
            <slot v-else>
              {{ $t('message.flowsView.graph.notSet') }}
            </slot>
          </span>
        </p>
      </div>
      <!-- Queues -->
      <div v-for="(route, routeIdx) in rule.routes" :ref="queueRefId(ruleIdx)"
           :style="`top:${ruleIdx * 175 + 76}px;left:${routeIdx * (250 + 200) + 620}px`"
           class="Cr-rules__item Cr-rules__item--queue">
        <div>
          <vlt-icon icon="stack" class="Vlt-grey" />
          <p class="Cr-rules__item__desc">
            <b>{{ $t('message.flowsView.graph.queue')}}</b>
          </p>
          <p class="Cr-rules__item__desc" place="queueRef">
            <slot v-if="route.queueRef">{{ route.queueRef }}</slot>
            <slot v-else>{{ $t('message.flowsView.graph.notSet') }}</slot>
          </p>
        </div>
      </div>
    </slot>
  </div>
</template>

<script>
  import Vue from 'vue';
  import {jsPlumb} from 'jsplumb';
  import BIcon from "buefy/src/components/icon/Icon";
  import { VltBadge, VltIcon } from "../../assets/volta/vue";

  const VltIconClass = Vue.extend(VltIcon);

  export default {
    components: {
      VltBadge,
      VltIcon
    },

    name: "flow-graph",

    props: {
      graph: Object,
      addRule: Function,
      editRule: Function,
      addAction: Function,
      editRoute: Function,
      editDefaultRoute: Function,
    },

    data() {
      return {
        // jsPlumb instance
        instance: {}
      };
    },

    computed: {},

    watch: {},

    methods: {

      ruleRefId: function () {
        return `rule`;
      },

      ruleRef: function (idx) {
        let ref = this.ruleRefId();
        return this.getRef(ref)[idx];
      },

      queueRefId: function (ruleIdx) {
        return `queue-${ruleIdx}`;
      },

      queueRef: function (ruleIdx, routeIdx) {
        let ref = this.queueRefId(ruleIdx);
        return this.getRef(ref)[routeIdx];
      },

      actionRefId: function (ruleIdx) {
        return `action-${ruleIdx}`;
      },

      actionRef: function (ruleIdx, routeIdx) {
        let ref = this.actionRefId(ruleIdx, routeIdx);
        return this.getRef(ref)[routeIdx];
      },

      getRef: function (ref) {
        return this.$refs[ref];
      },

      initDnd: function () {
        // let els = document.querySelectorAll('.item');
        // this.instance.draggable(els, {
        //   containment: true,
        //   grid: [50, 50],
        //   snapThreshold: 100,
        //   start: function (params) {
        //     // console.log("Start drag", params);
        //   },
        //   drag: function (params) {
        //     // console.log("Dragging", params);
        //     // params.drag.k.snapToGrid(50, 50);
        //   },
        //   stop: function (params) {
        //     // console.log("Stop drag", params);
        //   },
        // });
        //
        // this.instance.droppable(els, {
        //   drop: function (params) {
        //     // console.log("drop", params);
        //
        //     const drag = params.drag;
        //     const drop = params.drop;
        //     const event = params.e;
        //
        //     // do some codes
        //     return true;
        //   }
        // });

        // this.instance.bind("beforeDrop", function (params) { // checkDropAllowed
        //
        //   console.log("drop allowed:", params);
        //
        //   // Here you have access to:
        //   // params.sourceEndpoint
        //   // params.targetEndpoint
        //   // params.connection
        //
        //   return true; // or false.  in this case we say drop is allowed.
        // });
      },

      createSource: function (el, options = {}) {
        return this.instance.addEndpoint(
          el,
          options,
          {
            isSource: true,
            isTarget: true,
            detachable: false,
            // anchor: "Right",
            endpoint: "Blank" // ["Rectangle", {width: 10, height: 10}]
          });
      },

      createTarget: function (el, options = {}) {
        return this.instance.addEndpoint(
          el,
          options,
          {
            isSource: true,
            isTarget: true,
            detachable: false,
            // anchor: "Left",
            endpoint: "Blank"
          });
      },

      connect: function (source, target, options = {}) {
        return this.instance.connect(
          {
            source: source,
            target: target,
            connector: ["Flowchart", {cornerRadius: 5}],
            overlays: [
              ['Arrow', {width: 12, height: 5, location: 1, cssClass: 'arrow'}],
            ]
          },
          options);
      },

      initDefaultRuleConnections: function () {
        // Default Rule -> Default Route
        let defaultRuleElement = this.getRef('defaultRule');
        let defaultRouteElement = this.getRef('defaultRoute');
        let defaultQueueElement = this.getRef('defaultQueue');
        const posseId = `posse-default-rule`;
        const elements = [defaultRuleElement, defaultRouteElement, defaultQueueElement];
        this.instance.addToPosse(elements, posseId);

        let defaultRuleEndpoint = this.createSource(defaultRuleElement, {anchor: "Right"});
        let defaultRouteEndpointTarget = this.createTarget(defaultRouteElement, {anchor: "Left"});
        this.connect(defaultRuleEndpoint, defaultRouteEndpointTarget);

        // Default Route -> Default Queue
        let defaultRouteEndpointSource = this.createSource(defaultRouteElement, {anchor: "Right"});
        let defaultQueueEndpoint = this.createTarget(defaultQueueElement, {anchor: "Left"});
        this.connect(defaultRouteEndpointSource, defaultQueueEndpoint);
      },

      initGraph: function () {
        this.initDnd();

        this.instance.addGroup({
          el: this.$refs.ruleSet,
          id: 'ruleSetGroup',
          draggable: false,
          collapsed: false
        });

        this.initDefaultRuleConnections();

        this.graph.rules.forEach((rule, idx, collection) => {
          this.addRuleToGraph(rule, idx, collection);
        });
      },

      addRuleToGraph: function (rule, ruleIdx, rulesCollection) {
        let that = this;
        let ruleElement = this.ruleRef(ruleIdx);
        const posseId = `posse-${ruleIdx}`;
        this.instance.addToPosse(ruleElement, posseId);

        this.instance.addToGroup("ruleSetGroup", ruleElement);

        if (rule.routes.length === 0) {
          this.createSource(ruleElement, {
            anchor: "Right",
            endpoint: "Blank",
            overlays: [
              ['Custom', {
                create: this.createAddAction,
                cssClass: 'Vlt-btn Vlt-btn--icon Vlt-btn--app Vlt-btn--tertiary Cr-rules__add',
                location: [1, 0]
              }]
            ]
          })
          .bind("click", function (endpoint, originalEvent) {
            that.onAddAction(rule, ruleIdx);
          });
        }

        let sourceEndpoint;
        let targetEndpoint;
        if (ruleIdx < rulesCollection.length - 1) {
          let currRuleElement = this.ruleRef(ruleIdx);
          let nextRuleElement = this.ruleRef(ruleIdx + 1);
          sourceEndpoint = this.createSource(currRuleElement, {anchor: "Bottom"});
          targetEndpoint = this.createTarget(nextRuleElement, {anchor: "Top"});
        }

        if (ruleIdx === rulesCollection.length - 1) {
          let currRuleElement = this.ruleRef(ruleIdx);
          let defaultRuleElement = this.getRef('defaultRule');
          sourceEndpoint = this.createSource(currRuleElement, {anchor: "Bottom"});
          targetEndpoint = this.createTarget(defaultRuleElement, {anchor: "Top"});
        }

        this.connect(sourceEndpoint, targetEndpoint, {
          overlays: [
            ['Arrow', {width: 12, height: 5, location: 1, cssClass: 'arrow'}],
            ['Label', {label: 'else', cssClass: 'middle-of-line', location: 0.5}]
          ]
        });

        rule.routes.forEach((route, routeIdx, collection) => {
          this.addActionToGraph(rule, ruleIdx, route, routeIdx, collection);
        });
      },

      addActionToGraph: function (rule, ruleIndex, route, routeIdx, collection) {
        let that = this;
        let ruleElement = this.ruleRef(ruleIndex);
        const posseId = `posse-${ruleIndex}`;
        let actionElement = this.actionRef(ruleIndex, routeIdx);
        let queueElement = this.queueRef(ruleIndex, routeIdx);
        this.instance.addToPosse([actionElement, queueElement], posseId);

        // Action -> Queue
        let toQueueEndpoint = this.createSource(actionElement, {anchor: "Right"});
        let fromActionEndpoint = this.createTarget(queueElement, {anchor: "Left"});
        this.connect(toQueueEndpoint, fromActionEndpoint);

        if (routeIdx === 0) {
          let toFirstActionEndpoint = this.createSource(ruleElement, {anchors: "Right"});
          let fromRuleEndpoint = this.createTarget(actionElement, {anchors: "Left"});
          this.connect(toFirstActionEndpoint, fromRuleEndpoint);
        }

        if (collection.length - 1 === routeIdx) {
          // Last element
          let lastEndpoint = this.createSource(queueElement, {
            anchor: "Right",
            endpoint: "Blank",
            overlays: [
              ['Custom', {
                create: this.createAddAction,
                cssClass: 'Vlt-btn Vlt-btn--icon Vlt-btn--app Vlt-btn--tertiary Cr-rules__add',
                location: [1, 0]
              }]
            ]
          });
          lastEndpoint.bind("click", function (endpoint, originalEvent) {
            that.onAddAction(rule, ruleIndex);
          });
        }

        if (routeIdx - 1 >= 0) {
          let prevQueueElement = this.queueRef(ruleIndex, routeIdx - 1);
          let queueEndpoint = this.createSource(prevQueueElement, {anchor: "Right"});
          let actionEndpoint = this.createTarget(actionElement, {anchor: "Left"});
          this.connect(queueEndpoint, actionEndpoint, {
            overlays: [
              ['Arrow', {width: 12, height: 5, location: 1, cssClass: 'arrow'}],
              ['Custom', {
                create: this.createTimeout,
                cssClass: 'clock middle-of-line',
                location: 0.5
              }]
            ]
          });
        }
      },

      moveRuleUp: function (idx) {
        const rule = this.graph.rules.splice(idx, 1)[0];
        this.graph.rules.splice(idx - 1, 0, rule);
      },

      moveRuleDown: function (idx) {
        const rule = this.graph.rules.splice(idx, 1)[0];
        this.graph.rules.splice(idx + 1, 0, rule);
      },

      moveActionLeft: function (ruleIdx, actionIdx) {
        const action = this.graph.rules[ruleIdx].routes.splice(actionIdx, 1)[0];
        this.graph.rules[ruleIdx].routes.splice(actionIdx - 1, 0, action);
      },

      moveActionRight: function (ruleIdx, actionIdx) {
        const action = this.graph.rules[ruleIdx].routes.splice(actionIdx, 1)[0];
        this.graph.rules[ruleIdx].routes.splice(actionIdx + 1, 0, action);
      },

      createAddAction: function (component) {
        return this.createIcon('plus', 'addAction');
      },

      createTimeout: function (component) {
        return this.createIcon('clock');
      },

      createIcon: function (iconName, tooltip = null, i18nPrefix = 'message.flowsView.graph.tooltip.') {
        const span = document.createElement('span');
        if (tooltip) {
          let text = this.$i18n.t(i18nPrefix + tooltip);
          span.setAttribute('data-label', text);
        }
        const vltIconClass = new VltIconClass({
          propsData: { icon: iconName, size: 'small', color: 'blue' }
        });
        vltIconClass.$mount();
        span.appendChild(vltIconClass.$el);
        return span;
      },

      onEditRule: function (rule, index) {
        if (typeof this.editRule === 'function') {
          this.editRule(rule, index);
        }
      },

      onAddRule: function () {
        if (typeof this.addRule === 'function') {
          this.addRule();
        }
      },

      onAddAction: function (rule, index) {
        if (typeof this.addAction === 'function') {
          const that = this;
          this.addAction(rule, index);
        }
      },

      onEditAction: function (rule, ruleIdx, route, routeIdx) {
        if (typeof this.editRoute === 'function') {
          this.editRoute(rule, ruleIdx, route, routeIdx);
        }
      },
    },

    mounted() {
      const that = this;
      jsPlumb.ready(function () {

        that.instance = jsPlumb.getInstance({
          // DragOptions: {cursor: 'grab', zIndex: 2000},
          Container: that.$el,
        });

        that.initGraph();
      });
    },

    updated() {
      const that = this;
      that.instance.empty("defaultRule");
      that.instance.deleteEveryEndpoint();
      that.initDnd();
      that.initDefaultRuleConnections();
      that.graph.rules.forEach((r, idx, c) => {
        that.addRuleToGraph(r, idx, c);
      });
    },

    beforeDestroy() {
      this.instance.reset();
    }
  }
</script>

<style lang="scss" scoped>
  @import "~assets/style/_styles.scss";
  @import "~assets/volta/scss/lib/_variables.scss";
  @import "~assets/volta/scss/lib/_mediaqueries.scss";

  $box-shadow: 0 1px 20px rgba(44, 45, 48, 0.1);

  .jtk-drag-select * {
    -webkit-touch-callout: none;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
  }

  .jtk-connector {
    z-index: 4;
  }

  .jtk-endpoint {
    z-index: 5;
  }

  .jtk-overlay {
    z-index: 6;
  }

  .jtk-dragged {
    z-index: 1000;
  }

  .jsPlumbContainer {
    height: calc(100vh - 300px);
    margin-top: $unit1;
    position: relative;
    overflow: auto;
  }

  .Cr-rules {
    border-radius: 10px;
    background-color: $grey-lighter;
    border: 1px solid $grey-light;
    padding: 10px;
    position: relative;
    height: calc(100vh - 340px);
    width: 290px;

    h3 {
      margin-top: 10px;
      margin-left: 10px;
    }

    .button.add-rule {
      margin-top: 10px;
      margin-right: 10px;
    }
  }

  .Cr-rules__item {
    left: 10px;
    height: 115px;
    width: 250px;
    border: 1px solid $grey-light;
    float: left;
    position: absolute;
    cursor: default; /* grab;*/
    border-radius: 10px;
    background-color: white;
    box-shadow: $box-shadow;
    
    .edit-icon {
      cursor: pointer;
    }

    .Cr-rules__item__title {
      display: block;
      text-align: left;
      padding: 10px;
      height: 40px;
      border-radius: 8px 8px 0 0;
      background-color: $blue-dark;
      color: $white;
      margin-bottom: 5px;

      &--truncated {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
    }

    .Cr-rules__item__desc {
      margin-bottom: 0;
      padding: 0 10px;
    }

    span {
      word-wrap: break-word;
    }
  }

  .group {
    height: 480px;
    width: 300px;
    position: absolute;
  }

  .Cr-rules__item--queue {
    width: 120px;
    height: 95px;
    text-align: center;
    border-radius: 5px;
    box-shadow: $box-shadow;
    padding: 10px;

    .queue-icon {
      margin-bottom: 3px
    }

    .queue-text {
      line-height: 1.2rem;

      div {
        text-overflow: ellipsis;
        overflow: hidden;
        white-space:nowrap;
      }
    }
  }

  .bold {
    font-weight: bold;
  }

  .final {
    display: flex;
    align-items: center;
    justify-content: center;

    .icon {
      cursor: pointer;
    }
  }

  .tooltip:hover,
  .tooltip:hover:before,
  .tooltip:hover:after {
    z-index: 1000;
  }

  

</style>
