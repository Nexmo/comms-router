<template>
  <vlt-modal :title="getTitle()" :visible="true" large :extra-btn-label="isEdit ? $t('message.queuesView.delete.title'): null" extra-btn-class="Vlt-btn--destructive"
              extra-btn-icon="bin" @extraBtnClick="deleteBtn" role="dialog" large @confirm="ok" @close="close" :okText="isEdit ? $t('message.save') : $t('message.create')" :cancel-text="$t('message.cancel')">

      <vlt-field :label="$t('message.name')">
        <vlt-input v-model="newQueue.ref" :val="newQueue.ref" v-focus></vlt-input>
      </vlt-field>

      <vlt-field :label="$t('message.description')">
        <vlt-input v-model="newQueue.description" :val="newQueue.description"></vlt-input>
      </vlt-field>

      <label class="Vlt-label">{{$t('message.queuesView.predicate')}}</label>
      <vlt-field :label="$t('message.raw')">
        <vlt-switch v-model="rawPredicate" :checked="rawPredicate" />
      </vlt-field>

      <vlt-field v-if="rawPredicate">
        <vlt-input id="predicate" v-model="newQueue.predicate" :val="newQueue.predicate" textarea></vlt-input>
      </vlt-field>

      <vue-query-builder
        v-else
        :rules="{}"
        :maxDepth="10"
        :labels="{}"
        :rsqlQuery="newQueue.predicate"
        :routerRef="newQueue.routerRef"
        @rsqlError="onRqslError"
        @queryUpdated="queryUpdated">
      </vue-query-builder>

      <p v-if="errors.rsql" class="help is-danger">{{errors.rsql}}</p>
    </div>

  </vlt-modal>
</template>

<script>
  import VueQueryBuilder from '../../skills-query-builder/VueQueryBuilder';
  import { VltField, VltInput, VltModal, VltSwitch } from '../../../assets/volta/vue';

  const fields = ['ref', 'predicate', 'description'];

  export default {
    name: 'queue-modal',

    components: {
      VltField,
      VltInput,
      VltModal,
      VueQueryBuilder,
      VltSwitch
    },

    props: {
      visible: Boolean,
      title: String,
      queue: Object
    },

    data() {
      return {
        newQueue: {},
        isEdit: undefined,
        errors: {},
        rawPredicate: false
      }
    },

    beforeMount () {
      this.isEdit = !!this.queue.ref;
      this.newQueue = Object.assign({}, this.queue);
      if(!this.newQueue.routerRef) {
        this.newQueue.routerRef = this.$store.state.app.routerRef.ref;
      }
    },

    watch: {
      'newAgent.address': function (newAddr, oldAddr) {
        this.formatAddressOnType();
      },
      rawPredicate: function(val) {
        if (!val) {
          this.$delete(this.errors, 'rsql');
        }
      }
    },

    methods: {
      getTitle: function () {
        return this.queue != null ? this.$i18n.t('message.queuesView.edit') : this.$i18n.t('message.queuesView.create')
      },

      ok: function () {
        this.$delete(this.errors, 'predicate');
        // if (!this.newQueue.predicate) {
        //   this.$set(this.errors, 'predicate', this.$i18n.t('message.errors.emptyField'));
        //   return;
        // }

        fields.forEach(f => {
          if (this.newQueue[f] === this.queue[f]) {
            delete(this.newQueue[f]);
          }
        });

        Object.keys(this.newQueue)
          .filter(f => !fields.includes(f))
          .forEach(f => delete(this.newQueue[f]));

        console.log("update:", this.newQueue);

        if(this.isEdit) {
          this.$emit('update', this.newQueue);
        } else {
          this.$emit('add', this.newQueue);
        }
      },

      deleteBtn: function () {
        if (this.isEdit) {
          this.$emit('delete', this.queue);
        }
      },

      close() {
        this.$emit('close');
      },

      queryUpdated: function(newQuery) {
        let predicate = this.newQueue.predicate ? this.newQueue.predicate.toLowerCase() : '';
        if (!this.errors.rsql && predicate !== 'true' && predicate !== 'false') {
          this.newQueue.predicate = newQuery;
        }
      },

      onRqslError: function() {
        let predicate = this.newQueue.predicate.toLowerCase();
        if (predicate !== 'true' && predicate !== 'false') {
          this.$set(this.errors, 'rsql', this.$i18n.t('message.errors.failedParsePredicate'));
        }
        this.$nextTick(function () {
          this.rawPredicate = true;
        });
      }
    }
  }
</script>

<style scoped>
</style>
