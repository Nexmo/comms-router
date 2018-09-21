<template>
  <section>
    <div class="Vlt-grid">
      <div class="Vlt-col">
        <h1 class="Vlt-title--icon"><vlt-icon icon="mind-map" class="Vlt-blue"/>{{$t('message.skills')}}</h1>
      </div>
      <div class="Vlt-col Vlt-col--right">
        <button class="Vlt-btn Vlt-btn--app Vlt-btn--secondary" @click="skillVisible = true">
          <vlt-icon icon="plus" />
          <span>{{ $t('message.skillsView.add') }}</span>
        </button>
      </div>
    </div>

    <div class="Vlt-grid">
      <div class="Vlt-col Vlt-col--1of3">
        <cr-search v-model="search" />
      </div>
    </div>

    <vlt-table :columns="columns" @click="editSkill" :rows="rows" pagination>
      <template slot="item" slot-scope="slotProps">
        <td>{{ slotProps.item.ref }}</td>
        <td>{{ slotProps.item.description }}</td>
        <td> {{ getSkillType(slotProps.item.domain.type) }}</td>
        <td>
          <vlt-badge v-for="value in slotProps.item.domainCollection" v-if="value" :key="value" color="green">
              {{value}}
          </vlt-badge>
        </td>
      </template>
    </vlt-table>

    <skill-modal :skill="skill" v-if="skillVisible" :visible="skillVisible" @close="closeModal" @add="addSkill" @update="updateSkill" @delete="deleteSkill"></skill-modal>
  </section>
</template>

<script>
  import { getSkill, getSkills, createSkill, updateSkill, deleteSkill } from '../../api/api-client';
  import _ from 'lodash';
  import SkillModal from './modals/SkillModal';
  import { confirmDelete, notifyError } from "../../utils/Utils";
  import CrSearch from "../../components/CrSearch";

  import { VltBadge, VltField, VltIcon, VltComposite, VltTable } from '../../assets/volta/vue';

  export default {
    components: {
      SkillModal,
      CrSearch,
      VltBadge,
      VltField,
      VltIcon,
      VltComposite,
      VltTable
    },

    data() {
      return {
        routerId: this.$store.state.app.routerRef,
        isLoading: false,
        tokens: [''],
        search: '',
        totalCount: 11,
        currentPage: 1,
        columns: [
          { title: 'Name', property: 'ref', sortable:true },
          { title: 'Description', property: 'description', sortable:true },
          { title: 'Type', property: 'domain.type', sortable:true },
          { title: 'Values', property: 'domainCollection' }
        ],
        perPage: 20,
        rows: [],
        skillVisible: false,
        skill: undefined,
        sortField: 'ref',
        sortOrder: 'asc'
      }
    },

    methods: {
      onPageChange: function (page) {
        if (page !== this.currentPage) {
          this.currentPage = page;
          this.retrieveSkills();
        }
      },

      onSort (field, order) {
        this.currentPage = 1;
        this.token = [''];
        this.sortField = 'ref';//field;
        this.sortOrder = order;
        this.retrieveSkills();
      },

      parseValues : function (values) {
        let valuesStr = '';
        if (values) {
            for (let value in values) {
                valuesStr = valuesStr + values[value] + ', ';
            }
            if (valuesStr !== '') {
                valuesStr = valuesStr.slice(0, -2);
            }
        }
        return valuesStr;
      },

      parseIntervals : function (intervals) {
        let intervalString = '';
        let collection = [];
        for (let i in intervals) {
            let interval = intervals[i];
            if (interval.low && interval.low.inclusive) {
                intervalString += '[' + interval.low.boundary + ','  + (interval.high ? interval.high.boundary : 'Infinity');
            } else {
                intervalString += '(' + interval.low.boundary + ', ' + (interval.high ? interval.high.boundary : 'Infinity');
            }
            if (interval.high && interval.high.inclusive) {
                intervalString += ']';
            } else {
                intervalString += ')';
            }
            collection.push(intervalString);
            intervalString = '';
        }
        return collection;
      },

      parseDomain : function (domain) {
        let collection;
        switch (domain.type) {
            case 'enumeration':
                collection = domain.values;
                break;
            case 'number':
                collection = this.parseIntervals(domain.intervals);
                break;
            case 'string':
                collection = [domain.regex];
                break;
        }
        return collection;
      },

      retrieveSkills: function () {
        this.isLoading = true;
        const sort = `${this.sortOrder === 'asc' ? '+' : '-'}${this.sortField}`;

        let searchStr = '';
        if (this.search) {
          let searchEsc = this.search.replace("'", "\\'");
          searchStr = `ref=='*${searchEsc}*' or description=='*${searchEsc}*'`
        }

        const token = this.tokens[this.currentPage - 1];

        this.$router.replace({query: {sort: sort}});

        getSkills(this.routerId, token, this.perPage, searchStr, sort)
        .then((response) => {
          this.isLoading = false;
          if (response.data.length === 0 && this.currentPage > 1) {
            this.totalCount--;
            this.currentPage--;
            this.$toast.open(this.$t('message.endOfRecords'));
            return;
          }

          this.tokens[this.currentPage] = response.nextToken;
          this.totalCount = (this.currentPage - 1) * this.perPage + response.data.length;
          if (response.data.length === this.perPage) {
            this.totalCount++;
          }

          for (let row of response.data) {
              row.domainCollection = this.parseDomain(row.domain);
          }

          this.rows = response.data;

        })
        .catch(response => {
          console.error(response);
          this.isLoading = false;
          notifyError(this, response, this.$i18n.t('message.skillsView.errors.failedRetrieve'));
        });
      },

      addSkill: function (skill) {
        const self = this;

        return createSkill(self.routerId, skill)
          .catch((e) => notifyError(self, e, self.$i18n.t('message.skillsView.errors.failedCreate')))
          .finally(function() {
            self.closeModal();
            self.retrieveSkills();
          });
      },

      closeModal: function() {
        this.skill = undefined;
        this.skillVisible = false;
      },

      editSkill: async function (row) {
        const that = this;
        const skill = await getSkill(row);

        if(skill.length === 1) {
          this.skill = skill[0];
        } else {
          this.skill = row;
        }

        this.skillVisible = true;
      },

      deleteSkill: function(skill) {
        this.closeModal();

        const self = this,
              title = this.$i18n.t("message.skillsView.delete.title"),
              message = this.$i18n.t("message.skillsView.delete.prompt", skill);

        confirmDelete(self, title, message, () => {
          deleteSkill(skill.routerRef, skill.ref).then(res => {
              self.retrieveSkills();
          });
        });
      },

      updateSkill: async function(skillEdits) {
        const self = this;

        //api is expecting an array
        if(skillEdits.domain.values && !_.isArray(skillEdits.domain.values)) {
          skillEdits.domain.values = _.map(skillEdits.domain.values.split(","), (val) => {
            return _.trim(val);
          });
        }

        return updateSkill(self.skill, skillEdits)
          .catch((e) => notifyError(self, e, self.$i18n.t('message.skillsView.errors.failedEdit')))
          .finally(function(){
            self.closeModal();
            self.retrieveSkills();
          });
      },

      performSearch: _.debounce(function () {
        this.currentPage = 1;
        this.token = [''];
        this.retrieveSkills();
      }, 500),

      getSkillType: function(type) {
        if (!type) {
          return '';
        }

        switch (type.toLowerCase()) {
          case 'string':
            return this.$i18n.t('message.skillsView.typeText');
          case 'enumeration':
            return this.$i18n.t('message.skillsView.typeEnum');
          case 'number':
            return this.$i18n.t('message.skillsView.typeNumber');
          case 'bool':
            return this.$i18n.t('message.skillsView.typeBoolean');
        }
        return '';
      }
    },

    watch: {
      search: function () {
        this.performSearch();
      }
    },

    mounted() {
      this.routerId = this.$route.params.routerRef;
      this.retrieveSkills();
    }
  }
</script>

<style lang="scss" scoped>
</style>
