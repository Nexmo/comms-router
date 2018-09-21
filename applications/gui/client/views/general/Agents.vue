<template>
  <section>
    <div class="Vlt-grid">
      <div class="Vlt-col">
        <h1 class="Vlt-title--icon"><vlt-icon icon="receptionist" class="Vlt-blue"/></svg>{{$t('message.agents')}}</h1>
      </div>
      <div class="Vlt-col Vlt-col--right">
        <button class="Vlt-btn Vlt-btn--app Vlt-btn--secondary" @click="agent = {}, agentVisible = true">
          <vlt-icon icon="plus" />
          <span>{{ $t('message.agentsView.add') }}</span>
        </button>
      </div>
    </div>

    <div class="Vlt-grid">
      <div class="Vlt-col Vlt-col--1of3">
        <cr-search v-model="search" />
      </div>
    </div>

    <vlt-table :columns="columns" @click="editAgent" :rows="rows" pagination>
      <template slot="item" slot-scope="slotProps">
        <td>{{ slotProps.item.name }}</td>
        <td>{{ slotProps.item.description }}</td>
        <td>
          <flag :squared="false" :iso="slotProps.item.formatted ? slotProps.item.formatted.country : ''"/>
          {{ slotProps.item.formatted ? slotProps.item.formatted.address : slotProps.item.address }}
        </td>
        <td>
          <span v-for="(capability, index) in slotProps.item.capabilitiesParsed"
                :key="capability"
                class="Vlt-badge-group">
            <!--Keep these badges butted up against easchother to prevent whitespace-->
            <vlt-badge stacked color="grey">{{capability.split(':')[0]}}</vlt-badge><vlt-badge color="green">{{capability.split(':')[1]}}</vlt-badge>
          </span>
        </td>
        <td>
           <span v-for="qRef in slotProps.item.queueRefs" class="tag is-warning is-small tag-list">
              {{qRef}}
            </span>
        </td>
      </template>
    </vlt-table>

    <agent-modal :agent="agent" v-if="agentVisible" :visible="agentVisible" :routerRef="routerId"
                 @close="closeModal" @add="addAgent" @update="updateAgent" @delete="deleteAgent"/>

  </section>
</template>

<script>
  import { getAgent, getAgents, createAgent, updateAgent, deleteAgent } from '../../api/api-client';
  import AgentModal from './modals/AgentModal'
  import * as libPhoneNumber from 'google-libphonenumber';
  import _ from 'lodash';
  import { confirmDelete, notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";

  import { VltBadge, VltField, VltIcon, VltTable } from '../../assets/volta/vue';

  const PhoneNumberUtil = libPhoneNumber.PhoneNumberUtil.getInstance();
  const PhoneNumberFormat = libPhoneNumber.PhoneNumberFormat;

  export default {
    components: {
      AgentModal,
      CrSearch,
      VltBadge,
      VltField,
      VltIcon,
      VltTable
    },

    data() {
      return {
        agent: undefined,
        agentVisible: false,
        routerId: this.$store.state.app.routerRef,
        isLoading: false,
        perPage: 10,
        sortField: 'name',
        sortOrder: 'asc',
        tokens: [''],
        search: '',
        totalCount: 11,
        currentPage: 1,
        sortableFields: ['name', 'description', 'address'],
        rows: [],
        columns: [
          {title: 'Name', property: 'name', sortable: true },
          {title: 'Description', property: 'description', sortable: true },
          {title: 'Phone / SIP'},
          {title: 'Skillset'},
          {title: 'Server Queues'}
        ]
      }
    },

    computed: {},

    methods: {
      closeAgentModal: function() {
        this.agentVisible = false;
      },

      onPageChange: function (page) {
        if (page !== this.currentPage) {
          this.currentPage = page;
          this.retrieveAgents();
        }
      },

      onSort: function (field, order) {
        this.currentPage = 1;
        this.tokens = [''];
        this.sortField = field;
        this.sortOrder = order;
        const sort = this.getSortUri(this.sortOrder, this.sortField);
        this.$router.replace({query: {sort: sort}});
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
          if(address.startsWith('00')) {
            address = address.slice(2, address.length);
          }
          if (!address.startsWith('+')) {
            address = '+' + address;
          }

          try {
            let parsedPhone = PhoneNumberUtil.parseAndKeepRawInput(address);
            country = PhoneNumberUtil.getRegionCodeForNumber(parsedPhone);
            formatted = PhoneNumberUtil.format(parsedPhone, PhoneNumberFormat.INTERNATIONAL);
          } catch (e) {
            // console.log("Cannot format phone number: ", address, "because:", e);
          }
          row.formatted = {
            address: formatted,
            country: country
          };
          return formatted;
        }
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

      retrieveAgents: function () {
        this.isLoading = true;
        const sort = this.getSortUri(this.sortOrder, this.sortField);

        let searchStr = '';
        if (this.search) {
          let searchEsc = this.search.replace("'", "\\'");
          searchStr = `name=='*${searchEsc}*' or description=='*${searchEsc}*' or address=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getAgents(this.routerId, token, this.perPage, searchStr, sort)
        .then((response) => {
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
          console.error(response);
          this.isLoading = false;
          notifyError(this, response, this.$i18n.t('message.agentsView.errors.failedRetrieve'));
        });
      },

      closeModal: function() {
        this.agent = undefined;
        this.agentVisible = false;
      },

      editAgent: async function (row) {
        const that = this;
        const agent = await getAgent(row);

        if(agent.length === 1) {
          this.agent = agent[0];
        } else {
          this.agent = row;
        }

        this.agentVisible = true;
      },

      addAgent: function (agent) {
        const self = this;
        return createAgent(self.routerId, agent)
          .catch((e) => notifyError(self, e, self.$i18n.t('message.agentsView.errors.failedCreate')))
          .finally(function() {
            self.retrieveAgents();
            self.closeModal();
          });
      },

      updateAgent: async function (agentUpdates) {
        const self = this;
        return updateAgent(self.agent, agentUpdates)
          .catch((e) => {
            notifyError(self, e, self.$i18n.t('message.agentsView.errors.failedEdit'));
          })
          .finally(function(){
             self.retrieveAgents();
             self.closeModal();
          });
      },

      deleteAgent: function (agent) {
        this.closeModal();

        const self = this,
              title = this.$i18n.t("message.agentsView.delete.title"),
              message = this.$i18n.t("message.agentsView.delete.prompt", agent);

        confirmDelete(self, title, message, () => {
          deleteAgent(agent.routerRef, agent.ref).then(res => {
              self.retrieveAgents();
          })
          .catch((e) => notifyError(that, e, that.$i18n.t('message.agentsView.errors.failedDelete')));
        });
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
    }
  }
</script>

<style lang="scss" scoped>
</style>
