<template>
  <section id="agents">
    <h1 class="Vlt-title--icon"><vlt-icon icon="flow" class="Vlt-purple" />{{$t('message.agents')}}</h1>

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
        <td>{{ slotProps.item.name }}</td>

        <td>
          <flag :squared="false" :iso="slotProps.item.formatted ? slotProps.item.formatted.country : ''"/>
          {{ slotProps.item.formatted ? slotProps.item.formatted.address : slotProps.item.address }}
        </td>

        <td>
          <div class="field is-grouped is-grouped-multiline">
            <span v-for="(capability, index) in slotProps.item.capabilitiesParsed"
                  :key="capability"
                  class="control">
              <span class="Vlt-badge-group">
                <vlt-badge stacked color="grey">{{capability.split(':')[0]}}</vlt-badge><vlt-badge color="green">{{capability.split(':')[1]}}</vlt-badge>
              </span>
            </span>
          </div>
        </td>

        <td>
          <vlt-dropdown-native>
              <select tabindex="0" v-model="slotProps.item.state"
                      :class="getStatusClass(slotProps.item.state)"
                      :disabled="slotProps.item.state === 'busy'"
                      @change="updateState(slotProps.item)">
                <option value="offline" :selected="slotProps.item.state === 'offline'"> Offline</option>
                <option value="ready" :selected="slotProps.item.state === 'ready'"> Ready</option>
                <option value="busy" :selected="slotProps.item.state === 'busy'" disabled> Busy</option>
                <option value="unavailable" :selected="slotProps.item.state === 'unavailable'" disabled>
                  Unavailable
                </option>
              </select>
          </vlt-dropdown-native>
        </td>

        <td>
          {{ slotProps.item.lastTimeAtBusyState ? $d(slotProps.item.lastTimeAtBusyState, 'short') : $t('message.never') }}
        </td>

        <td>
          <button class="Vlt-btn Vlt-btn--icon Vlt-btn--app Vlt-btn--tertiary" :disabled="slotProps.item.state !== 'ready'"
                  @click="showAgentAssistant(slotProps.item)">
            <VltIcon icon="devices" size="small" />
          </button>
        </td>

      </template>
    </vlt-table>

  </section>
</template>

<script>
  import { getAgents, setAgentState, findTask, updateTask } from '../../api/api-client';
  import { mapActions } from 'vuex';
  import * as libPhoneNumber from 'google-libphonenumber';
  import _ from 'lodash';
  import { notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";

  import { VltBadge, VltDropdownNative, VltField, VltInput, VltIcon, VltTable } from '../../assets/volta/vue';

  const PhoneNumberUtil = libPhoneNumber.PhoneNumberUtil.getInstance();
  const PhoneNumberFormat = libPhoneNumber.PhoneNumberFormat;

  // TODO format the address (phone), show notification for the update result, edit the refresh time
  export default {
    components: {
      CrSearch,
      VltDropdownNative,
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
        sortField: 'name',
        sortOrder: 'asc',
        defaultSortOrder: 'asc',
        sortableFields: ['name', 'address', 'state', 'lastTimeAtBusyState'],
        tokens: [''],
        search: '',
        totalCount:11,
        currentPage:1,

        refreshInterval: this.$store.state.app.monitoring.refreshIntervalAgents,
        refreshIntervalId: 0,

        rows: [],
        columns: [
          { title: 'Name', property: 'name', sortable: true },
          { title: 'Address' }, //should be sortable
          { title: 'Skillset' },
          { title: 'State' },
          { title: 'Last Busy', property: 'lastTimeAtBusyState', sortable: true },
          { title: 'Action' }
        ]
      }
    },

    methods: {
      ...mapActions([
        'setAgentsRefreshInt'
      ]),

      showAgentAssistant: function(agent) {
        window.open(`assistant.html#/assist/${this.routerId}/${agent.ref}`);
      },

      onPageChange: function(page) {
        this.currentPage = page;
        this.retrieveAgents();
      },

      onSort(field, order) {
        this.currentPage = 1;
        this.tokens = [''];
        this.sortField = field;
        this.sortOrder = order;
        this.retrieveAgents();
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

      updateState: function (agent) {
        // if (agent.state === 'busy') {
        //   findTask('router-ivr', 1,
        //     task => task.state === 'assigned' && task.agentRef === agent.ref)
        //   .then(task => {
        //     if (confirm(`You are going to complete task:${task.ref}`)) {
        //       updateTask('router-ivr', task.ref, {state: "completed"});
        //     }
        //   });
        // } else {
        setAgentState(this.routerId, agent, agent.state)
          .catch(error => notifyError(this, error, this.$i18n.t('message.agentsView.errors.failedSetState')));
        // }
      },

      retrieveAgents: function (isRecurring) {
        this.isLoading = true;
        const sort = this.getSortUri(this.sortOrder, this.sortField);

        let searchStr = '';
        if (this.search) {
          let searchEsc = this.search.replace("'", "\\'");
          searchStr = `name=='*${searchEsc}*' or address=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getAgents(this.routerId, token, this.perPage, searchStr, sort)
          .then(response => {
            this.isLoading = false;
            if (response.data.length === 0 && this.currentPage > 1) {
              this.totalCount--;
              this.currentPage--;
              this.$toast.open(this.$t('message.endOfRecords'));
              return;
            }

            for (let row of response.data) {
              row.capabilitiesParsed = this.parseCapabilities(row.capabilities);
              this.formatAddress(row);
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
            console.error(response);
            if (!isRecurring) {
              notifyError(this, response, this.$i18n.t('message.agentsView.errors.failedRetrieve'));
            }
          });
      },

      parseCapabilities: function (capabilities) {
        let parsedCaps = [];

        for (let key in capabilities) {
          let capability = capabilities[key];
          if (Array.isArray(capability)) {
            for (let subCap of capability) {
              parsedCaps.push(`${key}:${subCap}`);
            }
          } else {
            parsedCaps.push(`${key}:${capability}`);
          }
        }
        return parsedCaps;
      },

      formatAddress: function (row) {
        let address = row.address;
        if (!address || address.startsWith("sip:")) {
          row.formatted = {
            address: address,
            country: ''
          };
          return address;
        } else {
          let country = '';
          let formatted = address;
          if (address.startsWith('tel:') && address.indexOf(';') < 0) {
            address = address.slice(4, address.length);
          }

          if (!address.startsWith('+') && !address.startsWith('00')) {
            address = '+' + address;
          }

          try {
            let parsedPhone = PhoneNumberUtil.parseAndKeepRawInput(address);
            country = PhoneNumberUtil.getRegionCodeForNumber(parsedPhone);
            formatted = PhoneNumberUtil.format(parsedPhone, PhoneNumberFormat.INTERNATIONAL);
          } catch (e) { console.log(e); }

          row.formatted = {
            address: formatted,
            country: country
          };
          return formatted;
        }
      },

      clearRefreshInterval: function () {
        if (this.refreshIntervalId !== 0) {
          clearInterval(this.refreshIntervalId);
          this.refreshIntervalId = 0;
        }
      },

      setupRefreshAgentsInterval: function () {
        this.clearRefreshInterval();
        this.refreshIntervalId = setInterval(() => {
          this.retrieveAgents(true);
        }, this.refreshInterval * 1000);
      },

      getStatusClass: function(status) {
        if (!status) {
          return '';
        }

        switch (status.toLowerCase()) {
          case 'ready':
            return 'status-online';
          case 'offline':
            return 'status-offline';
          case 'busy':
            return 'status-busy';
        }
        return '';
      },

      isActiveStatus: function (currentStatus, status) {
        return currentStatus === status;
      },

      performSearch: _.debounce(function () {
        this.currentPage = 1;
        this.tokens = [''];
        this.retrieveAgents();
      }, 500)

    },

    watch: {
      search: function () {
        this.performSearch();
      },
      refreshInterval: function () {
        // if (this.refreshInterval > 60) {
        //   this.refreshInterval = 60;
        // } else if (this.refreshInterval < 5) {
        //   this.refreshInterval = 5;
        // }
        this.setAgentsRefreshInt(this.refreshInterval);
        this.setupRefreshAgentsInterval();
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
      this.retrieveAgents();
      this.setupRefreshAgentsInterval();
    },

    beforeDestroy() {
      this.clearRefreshInterval();
    }
  }
</script>

<style lang="scss" scoped>
</style>
