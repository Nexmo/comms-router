<template>
  <vlt-modal :title="isEdit ? defaultRoute ? $t('message.flowsView.editDefaultRoute') : $t('message.flowsView.editAction') : $t('message.flowsView.addAction')" @confirm="ok" @close="close"
              :extra-btn-label="isEdit && !defaultRoute ? $t('message.flowsView.deleteAction') : null"
              extra-btn-class="Vlt-btn--destructive" extra-btn-icon="bin" extra-btn-icon-pack="fa"
              @extraBtnClick="deleteBtn" role="dialog" :visible="visible">
    <section>
      <vlt-field :label="$t('message.flowsView.actionType')">
        <div class="Vlt-select">
          <select id="action-input" v-model="actionType" expanded disabled>
            <option v-for="type in actionTypes" :value="type">
              {{ $t(`message.flowsView.graph.actionTypes.${type}`) }}
            </option>
          </select>
        </div>
      </vlt-field>

      <vlt-field :label="$t('message.queue')">
        <vlt-autocomplete
          id="queue-input"
          v-model="modified.queueRef"
          :data="queues"
          field="ref"
          :loading="isFetching"
          @input="loadQueues">
        </vlt-autocomplete>
      </vlt-field>

      <vlt-field :label="$t('message.flowsView.priority')">
        <vlt-input type="number" ref="validate_priority" id="priority-input" v-model="modified.priority" :val="modified.priority" />
      </vlt-field>

      <vlt-field :label="$t('message.flowsView.timeout')">
        <vlt-input type="number" ref="validate_timeout" id="timeout-input" v-model="modified.timeout" :val="modified.timeout" />
      </vlt-field>
    </section>
  </vlt-modal>
</template>

<script>
  import Vue from 'vue';
  import BIcon from "buefy/src/components/icon/Icon";
  import BField from "buefy/src/components/field/Field";
  import BInput from "buefy/src/components/input/Input";
  import BSelect from "buefy/src/components/select/Select";
  import BAutocomplete from "buefy/src/components/autocomplete/Autocomplete";
  import {getQueues} from "../../../api/api-client";
  import config from 'buefy/src/utils/config';

  import { VltAutocomplete, VltField, VltInput, VltModal } from '../../../assets/volta/vue';

  export default {
    components: {
      BAutocomplete,
      BIcon,
      BSelect,
      BInput,
      BField,
      VltAutocomplete,
      VltField,
      VltInput,
      VltModal
    },

    props: {
      defaultRoute: {
        type: Boolean,
        default: false
      },
      visible: Boolean,
      title: String,
      route: Object,
      routerRef: String,
      onConfirm: Function,
      onCancel: Function,
      onDelete: Function
    },

    data() {
      return {
        modified: {},
        actionTypes: ['routeToQueue'],
        actionType: 'routeToQueue',
        isEdit: false,
        isCreate: Object.keys(this.route).length === 0 && this.route.constructor === Object,
        queues: [],
        queueName: '',
        isFetching: false
      }
    },

    mounted: function () {
      if (Object.keys(this.route).length > 0) {
        this.modified = Object.assign({}, this.route);
      }

      this.isEdit = !!this.route.queueRef;
    },

    methods: {

      ok: function () {
        if(this.defaultRoute) {
          this.$emit('updateDefault', this.modified);
        } else if(!this.isEdit) {
          this.$emit('add', this.modified);
        } else {
          this.$emit('update', this.modified);
        }
      },

      close() {
        this.$emit('close');
      },

      deleteBtn: function () {
        this.$emit('delete');
      },

      loadQueues: _.debounce(async function () {
        this.isFetching = true;
        let searchStr = this.modified.queueRef.replace("'", "\\'");
        searchStr = `ref=='*${searchStr}*'`;

        try {
          const response = await getQueues(this.routerRef, '', 20, searchStr);
          this.queues = response.data;
          this.isFetching = false;

          this.$nextTick(() => {
            document.querySelector('.Vlt-autocomplete .Vlt-dropdown__panel').scrollIntoView();
          });
        } catch (e) {
          this.isFetching = false;
        }
      }, 750)

    },
  }
</script>

<style lang="scss" scoped>
</style>
