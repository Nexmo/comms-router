<template>
  <div>
    <h1 class="Vlt-title--icon"><vlt-icon icon="stack" class="Vlt-purple" />{{$t('message.queues')}}</h1>

    <div class="Vlt-grid">
      <div class="Vlt-col Vlt-col--1of3">
        <cr-search v-model="search" />
      </div>
      <div class="Vlt-col"></div>
      <div class="Vlt-col Vlt-col--1of4">
        <vlt-field :label="$t('message.refreshInterval')" >
          <vlt-input id="refresh-interval" type="number" v-model.number="refreshInterval" :val="refreshInterval" maxlength="2" min="5" max="60" ></vlt-input>
        </vlt-field>
      </div>
    </div>

    <vlt-table :columns="columns" :rows="rows" pagination>
      <template slot="item" slot-scope="slotProps">
        <td>{{ slotProps.item.description }}</td>

        <td>
          <div>
            <span v-for="pred in slotProps.item.parsedPredicate">
              <vlt-badge color="green" v-if="pred && (pred.trim().length > 3 || pred === $t('message.queuesView.all') || pred === $t('message.queuesView.none'))">
                {{ pred }}
              </vlt-badge>
              <span v-else>{{pred.toUpperCase()}}</span>
            </span>
          </div>
        </td>

        <td>
          {{ slotProps.item.size }}
        </td>

        <td>
          <div class="field is-grouped is-grouped-multiline">
            <vlt-badge stacked :color="task.priority > 0 ? 'red' : 'blue'" v-for="(task, index) in slotProps.item.task" :key="task.ref" dismissable @dismissed="taskDelete(task, slotProps.index, index)">
              {{getLabel(task)}}
            </vlt-badge>
          </div>
        </td>
      </template>
    </vlt-table>
  </div>
</template>

<script>
  import { getQueues, getQueueTasks, cancelWaitingTask } from '../../api/api-client';
  import { mapActions } from 'vuex';
  import * as libPhoneNumber from 'google-libphonenumber';
  import _ from 'lodash';
  import { notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";

  import { VltBadge, VltField, VltInput, VltIcon, VltTable } from '../../assets/volta/vue';

  const PhoneNumberUtil = libPhoneNumber.PhoneNumberUtil.getInstance();
  const PhoneNumberFormat = libPhoneNumber.PhoneNumberFormat;
  /* eslint-disable */
  export default {
    components: {
      CrSearch,
      VltBadge,
      VltField,
      VltIcon,
      VltInput,
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
        sortableFields: ['description'],
        search: '',
        tokens: [''],
        refreshInterval: this.$store.state.app.monitoring.refreshIntervalQueues,
        refreshIntervalId: 0,

        columns: [
          { title: 'Description', property: 'description', sortable: true },
          { title: 'Predicate', property: 'predicate' },
          { title: 'Size', property: 'size', sortable: true },
          { title: 'Tasks', property: 'task' }
        ],
        rows: [],
        isLoading: false
      }
    },

    methods: {
      ...mapActions([
        'setQueuesRefreshInt'
      ]),

      onPageChange: function(page) {
        this.currentPage = page;
        this.retrieveQueues();
      },

      onSort(field, order) {
        this.currentPage = 1;
        this.tokens = [''];
        this.sortField = field;
        this.sortOrder = order;
        this.retrieveQueues();
      },

      getLabel(task) {
        let label;
        if(task.userContext && task.userContext.kind === 'callback' && task.userContext.callback_state==='completed') {
          label = this.$t('message.queuesView.callback_');
        } else {
          label = this.$t('message.queuesView.onHold_');
        }

        if(task.userContext) {
          label = `${label}: ${this.formatAddress(task.userContext.callback_number)}`
        }

        return label;
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

      clearRefreshInterval: function() {
        if (this.refreshIntervalId !== 0) {
          clearInterval(this.refreshIntervalId);
          this.refreshIntervalId = 0;
        }
      },

      setupRefreshQueuesInterval: function() {
        this.clearRefreshInterval();
        this.refreshIntervalId = setInterval(() => {
          this.retrieveQueues(true);
        }, this.refreshInterval * 1000);
      },

      retrieveQueues: function (isRecurring) {
        this.isLoading = true;
        const sort = this.getSortUri(this.sortOrder, this.sortField);

        let searchStr = '';
        if (this.search) {
            let searchEsc = this.search.replace("'", "\\'");
            searchStr = `ref=='*${searchEsc}*' or description=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getQueues(this.routerId, token, this.perPage, searchStr, sort)
                .then(async (response) => {
                  if (response.data.length === 0 && this.currentPage > 1) {
                    this.totalCount--;
                    this.currentPage--;
                    this.$toast.open(this.$t('message.endOfRecords'));
                    this.isLoading = false;
                    return;
                  }

                  for (let queue of response.data) {
                    await this.retrieveTasks4Queue(queue);
                  }
                  this.isLoading = false;

                  this.tokens[this.currentPage] = response.nextToken;
                  this.totalCount = (this.currentPage - 1) * this.perPage + response.data.length;
                  if (response.data.length === this.perPage) {
                    this.totalCount++;
                  }

                  this.rows = []; //force re-render, vue won't rerender if only properties are added to the rows
                  this.rows = response.data;
                })
                .catch(response => {
                    console.error(response);
                    this.isLoading = false;
                  if (!isRecurring) {
                    notifyError(this, response, this.$i18n.t('message.queuesView.errors.failedRetrieve'));
                  }
                });
        },

      retrieveTasks4Queue : async function(queue) {
        let tasks = await getQueueTasks(this.routerId, queue.ref);

        queue.parsedPredicate = queue.predicate ? queue.predicate.split(/( AND |;|,| OR )/gi): [];
        queue.parsedPredicate.forEach((el, index) => {
          el = el.toLowerCase();
          if (el === 'true') {
            queue.parsedPredicate[index] = this.$i18n.t('message.queuesView.all');
          } else if (el === 'false') {
            queue.parsedPredicate[index] = this.$i18n.t('message.queuesView.none');
          }
        });
        queue.task = tasks;
        queue.size = tasks ? tasks.length : 0;
      },

      taskDelete: async function(task, rowIndex, index) {
        try {
          await cancelWaitingTask(this.routerId, task.ref);

          let queue = this.rows[rowIndex];
          queue.task.splice(index, 1);
          queue.size--;

          this.$set(this.rows, rowIndex, queue);
        } catch (e) {
          console.log(e);
        }
      },

      formatAddress: function (address) {
        if (typeof address === 'undefined' || address.startsWith("sip:")) {
          return address;
        } else {
          if (!address.startsWith('+') || !address.startsWith('00')) {
            address = '+' + address;
          }
          let formatted = address;
          try {
            const parsedPhone = PhoneNumberUtil.parseAndKeepRawInput(address);
            formatted = PhoneNumberUtil.format(parsedPhone, PhoneNumberFormat.INTERNATIONAL);
          } catch (e) {console.log(e)}

          return formatted;
        }
      },

      performSearch: _.debounce(function () {
        this.currentPage = 1;
        this.tokens = [''];
        this.retrieveQueues();
      }, 500)
    },

    watch: {
      search: function () {
          this.performSearch();
      },
      refreshInterval: function() {
        if (this.refreshInterval > 60) {
          this.refreshInterval = 60;
        } else if (this.refreshInterval < 5) {
          this.refreshInterval = 5;
        }
        this.setQueuesRefreshInt(this.refreshInterval);
        this.setupRefreshQueuesInterval();
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
      this.retrieveQueues();
      this.setupRefreshQueuesInterval();
    },

    beforeDestroy () {
      this.clearRefreshInterval();
    }
  }
</script>

<style lang="scss" scoped>
  .nowrap {
    white-space: nowrap;
  }

  .title-table {
    display: flex;
    margin: 20px;
    justify-content: space-between;
  }

  .tag {
    margin: 5px;
  }

  .b-table {
    /deep/ small.info {
      display: none;
    }
    /deep/ tbody tr {
      cursor: auto;
    }
  }
</style>
