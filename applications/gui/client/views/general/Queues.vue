<template>
  <section>
    <div class="Vlt-grid">
      <div class="Vlt-col">
         <h1 class="Vlt-title--icon"><vlt-icon icon="stack" class="Vlt-blue"/>{{$t('message.queues')}}</h1>
      </div>
      <div class="Vlt-col--right">
        <button class="Vlt-btn Vlt-btn--app Vlt-btn--secondary" @click="queue = {}, queueVisible = true">
          <vlt-icon icon="plus" />
          <span>{{ $t('message.queuesView.add') }}</span>
        </button>
      </div>
    </div>

    <div class="Vlt-grid">
      <div class="Vlt-col Vlt-col--1of3">
        <cr-search v-model="search" />
      </div>
    </div>

    <vlt-table :columns="columns" @click="editQueue" :rows="rows" pagination>
      <template slot="item" slot-scope="slotProps">
        <td>{{ slotProps.item.ref }}</td>
        <td>{{ slotProps.item.description }}</td>
        <td>
          <div class="tags">
            <span v-for="pred in slotProps.item.parsedPredicate">
              <vlt-badge color="green" v-if="pred && (pred.trim().length > 3 || pred === $t('message.queuesView.all') || pred === $t('message.queuesView.none'))" small>
                {{ pred }}
              </vlt-badge>
              <span v-else>{{pred.toUpperCase()}}</span>
            </span>
          </div>
        </td>
      </template>
    </vlt-table>

    <queue-modal :queue="queue" v-if="queueVisible" :visible="queueVisible" @close="closeModal" @add="addQueue" @update="updateQueue" @delete="deleteQueue"></queue-modal>
  </section>
</template>

<script>
  import { getQueues, getQueue, createQueue, updateQueue, deleteQueue } from '../../api/api-client';
  import QueueModal from './modals/QueueModal';
  import _ from 'lodash';
  import { confirmDelete, notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";

  import { VltBadge, VltField, VltIcon, VltInput, VltTable } from '../../assets/volta/vue';

  export default {
    components: {
      CrSearch,
      QueueModal,
      VltBadge,
      VltField,
      VltInput,
      VltIcon,
      VltTable
    },

    data() {
      return {
        routerId: this.$store.state.app.routerRef,
        isLoading: false,
        perPage: 10,
        totalCount: 11,
        currentPage: 1,
        sortField: 'ref',
        sortOrder: 'asc',
        search: '',
        tokens: [''],
        rows: [],
        columns: [
          { title: 'Name', property: 'ref', sortable: true },
          { title: 'Description', property: 'description', sortable: true },
          { title: 'Agents Filter' },
        ],
        queue: undefined,
        queueVisible: false
      }
    },

    methods: {
      onPageChange: function (page) {
        if (page !== this.currentPage) {
          this.currentPage = page;
          this.retrieveQueues();
        }
      },

      onSort(field, order) {
        this.currentPage = 1;
        this.tokens = [''];
        this.sortField = field;
        this.sortOrder = order;
        this.retrieveQueues();
      },

      retrieveQueues: function () {
        this.isLoading = true;
        const sort = `${this.sortOrder === 'asc' ? '+' : '-'}${this.sortField}`;

        let searchStr = '';
        if (this.search) {
          let searchEsc = this.search.replace("'", "\\'");
          searchStr = `ref=='*${searchEsc}*' or description=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getQueues(this.routerId, token, this.perPage, searchStr, sort)
        .then(response => {
          this.isLoading = false;
          if (response.data.length === 0 && this.currentPage > 1) {
            this.totalCount--;
            this.currentPage--;
            this.$toast.open(this.$t('message.endOfRecords'));
            return;
          }

          for (let queue of response.data) {
            queue.parsedPredicate = queue.predicate ? queue.predicate.split(/( AND |;|,| OR )/gi): [];
            queue.parsedPredicate.forEach((el, index) => {
              el = el.toLowerCase();
              if (el === 'true') {
                queue.parsedPredicate[index] = this.$i18n.t('message.queuesView.all');
              } else if (el === 'false') {
                queue.parsedPredicate[index] = this.$i18n.t('message.queuesView.none');
              }
            });
          }

          this.tokens[this.currentPage] = response.nextToken;
          this.totalCount = (this.currentPage - 1) * this.perPage + response.data.length;
          if (response.data.length === this.perPage) {
            this.totalCount++;
          }

          this.rows = response.data;
        })
        .catch(response => {
          console.error(response);
          this.isLoading = false;
          notifyError(this, response, this.$i18n.t('message.queuesView.errors.failedRetrieve'));
        });
      },

      addQueue: function (queue) {
        const self = this;
        const queueRef = queue['ref'];
        delete(queue['ref']);

        return createQueue(this.routerId, queue, queueRef)
          .catch((e) => notifyError(self, e, self.$i18n.t('message.queuesView.errors.failedCreate')))
          .finally(function() {
            self.retrieveQueues();
            self.closeModal();
          });
      },

      closeModal: function() {
        this.queue = undefined;
        this.queueVisible = false;
      },

      editQueue: async function (row) {
        const that = this;
        const queue = await getQueue(row);

        if(queue.length === 1) {
          this.queue = queue[0];
        } else {
          this.queue = row;
        }

        this.queueVisible = true;
      },

      deleteQueue: function (queue) {
        this.closeModal();

        const self = this,
              title = this.$i18n.t("message.queuesView.delete.title"),
              message = this.$i18n.t("message.queuesView.delete.prompt", queue);

        confirmDelete(self, title, message, () => {
          deleteQueue(queue.routerRef, queue.ref).then(res => {
              self.retrieveQueues();
          }).catch(
            (e) => notifyError(self, e, self.$i18n.t('message.queuesView.errors.failedDelete')));
        });
      },

      updateQueue: async function(queueEdits) {
        const self = this;

        return updateQueue(this.queue, queueEdits)
          .catch((e) => notifyError(self, e, self.$i18n.t('message.queuesView.errors.failedEdit')))
          .finally(function() {
            self.closeModal();
            self.retrieveQueues();
          });
      },

      performSearch: _.debounce(function () {
        this.currentPage = 1;
        this.token = [''];
        this.retrieveQueues();
      }, 500)
    },
    watch: {
      search: function () {
        this.performSearch();
      }
    },
    mounted() {
      this.routerId = this.$route.params.routerRef;
      this.retrieveQueues();
    }
  }
</script>

<style lang="scss" scoped>
</style>
