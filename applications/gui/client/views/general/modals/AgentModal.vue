<template>
   <vlt-modal :title="getTitle()" :visible="true" large :extra-btn-label="isEdit ? $t('message.agentsView.delete.title'): null" extra-btn-class="Vlt-btn--destructive"
              extra-btn-icon="bin" @extraBtnClick="deleteBtn" role="dialog" large @confirm="ok" @close="close" :okText="isEdit ? $t('message.save') : $t('message.create')" :cancel-text="$t('message.cancel')">

      <vlt-field :label="$t('message.name')">
        <vlt-input id="name-input" :val="newAgent.name" v-model="newAgent.name" v-focus />
      </vlt-field>

      <vlt-field :label="$t('message.description')" optional>
        <vlt-input id="agent_descr" :val="newAgent.description" v-model="newAgent.description" maxlength="255" textarea />
      </vlt-field>

      <vlt-field :label="$t('message.agentsView.phoneOrSip')" optional>
        <div class="Vlt-composite">
          <div class="Vlt-composite__prepend Vlt-composite__prepend--icon">
            <flag :squared="false" :iso="addressCountry"/>
            <span class="flag--empty" v-if="!addressCountry"></span>
          </div>
          <div class="Vlt-input">
            <input id="address" type="text" v-model="newAgent.address"/>
          </div>
        </div>
      </vlt-field>

      <!-- Skills -->
      <h5 class="Vlt-label">{{$t('message.agentsView.capabilities')}}</h5>

      <div v-if="isFetchingInitSkills" class="loading is-loading is-large"></div>

      <div v-else v-for="(cap, index) in capabilitiesParsed">
         <!-- Existing skills -->
        <div v-if="cap.data && !(capabilitiesParsed.length === index + 1 && isAddingCapability)" class="Vlt-grid Cr-agent__skill">
          <div class="Vlt-col Vlt-col--1of4">
            <p v-if="cap.ref">{{cap.ref}}</p>
          </div>

          <div v-if="cap.multivalue" class="Vlt-col Vlt-col--1of2 Cr-agent__skills">
            <vlt-badge color="green" dismissable @dismissed="removeTag(cap, tag)" v-for="(tag, index) in cap.data" :key="index">{{tag}}</vlt-badge>
          </div>

          <div v-else class="Vlt-col Vlt-col--1of2 Cr-agent__skills">
            <vlt-badge color="green" v-for="tag in cap.data" :key="index">{{tag}}</vlt-badge>
          </div>

          <div class="Vlt-col Vlt-col--right Vlt-col--1of4 ">
            <button @click='editCapability(cap, index)' v-if="cap.multivalue && cap.ref" :disabled="modifiedCapability && modifiedCapability.ref === cap.ref"
                  :aria-label="$t('message.edit')" class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--small Vlt-btn--tertiary">
              <vlt-icon icon="edit" />
            </button>
            <button @click='removeCapability(index)' :disabled="!cap.ref || (modifiedCapability && modifiedCapability.ref === cap.ref)"
                  :aria-label="$t('message.skillsView.removeInterval')" class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--small Vlt-btn--tertiary">
              <vlt-icon icon="bin" />
            </button>
          </div>
        </div>

        <!-- Adding a new skill -->
        <div v-else class="Vlt-grid">
          <div class="Vlt-col Vlt-col--1of4">
            <vlt-dropdown 
              v-model="skillName"
              :options="skills"
              property="ref"
              :label="$t('message.skill')"
              @input="loadSkills"
              show-selection
              hide-label
            />
          </div>

          <div class="Vlt-col">
            <agent-capability :index="index"
                              :capability="cap"
                              @removeCapability="removeCapability"
                              @done="finishAddingCap"/>
          </div>
        </div>
      </div>

      <div class="Vlt-grid" v-if="isEditingCapability" >
        <div class="Vlt-col Vlt-col--1of4">
          <vlt-field v-else-if>
            <vlt-input disabled :val="modifiedCapability.ref" />
          </vlt-field>
        </div>
        <div class="Vlt-col">
          <agent-capability :capability="modifiedCapability" :isEdit="true" :index="modifiedCapabilityIndex" @removeCapability="removeCapability" @done="finishEditingCap" />
        </div>
      </div>

      <div class="Vlt-grid Vlt-grid--margin-bottom1">
        <div class="Vlt-col">
          <button v-if="!isFetchingInitSkills && !isAddingCapability && !isEditingCapability" class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary" @click='addCapability' :disabled="hasPendingSkill">
            {{ $t('message.agentsView.addCapability') }}
          </button>
        </div>
      </div>

      <div class="Vlt-callout Vlt-callout--critical" v-if="errors.cap && errors.cap.length > 0">
        <i></i>
        <ul class="Vlt-callout__content">
          <li v-for="error in errors.cap">{{error}}</li>
        </ul>
      </div>
  </vlt-modal>
</template>

<script>
  import _ from 'lodash';
  import Vue from 'vue';
  import * as libPhoneNumber from 'google-libphonenumber';
  import { getSkills, getSkill } from "../../../api/api-client";
  import AgentCapability  from '../../../components/AgentCapability';
  import { notifyError } from "../../../utils/Utils";
  import { VltModal, VltField, VltCheckbox, VltRadio, VltBadge, VltDropdown, VltInput, VltIcon, VltAutocomplete } from '../../../assets/volta/vue';

  const PhoneNumberUtil = libPhoneNumber.PhoneNumberUtil.getInstance();
  const PhoneNumberFormat = libPhoneNumber.PhoneNumberFormat;

  const fields = ['name', 'description', 'address', 'capabilities'];

  export default {
    components: {
      AgentCapability,
      VltAutocomplete,
      VltBadge,
      VltDropdown,
      VltModal,
      VltField,
      VltRadio,
      VltCheckbox,
      VltInput,
      VltIcon
    },

    props: {
      visible: Boolean,
      title: String,
      agent: Object,
      routerRef: String,
      onConfirm: Function,
      onCancel: Function,
      onDelete: Function
    },

    data() {
      return {
        newAgent: {},
        addressCountry: null,
        isAddingCapability: false,
        isEdit: false,
        capabilities: {},
        capVal: null,
        filteredSkillValues: [],

        capabilitiesParsed: [],
        skills: [],
        skillName: undefined,
        isFetchingSkills: false,
        errors: {},

        isEditingCapability: false,
        modifiedCapability: undefined,

        isFetchingInitSkills: false,
        fetchedInitSkills: 0
      }
    },

    mounted() {
      this.isEdit = !!this.agent.name;

      this.newAgent = Object.assign({}, this.agent);
      this.capabilities = this.newAgent.capabilities;
      this.parseCapabilities(this.capabilities);
      this.formatAddress();
    },

    computed: {
      isSaveDisabled: function() {
        // if (!this.newAgent.name || this.capabilitiesParsed.length === 0
        //     || (this.hasPendingSkill && this.capabilitiesParsed.length === 1)
        //     || !this.newAgent.address) {
        //   return true;
        // }
        return false;
      },

      hasPendingSkill: function () {
        const size  = this.capabilitiesParsed.length;
        if (size > 0 && this.capabilitiesParsed[size - 1].ref == null) {
          return true;
        }

        return false;
      }
    },

    watch: {
      'newAgent.address': function (newAddr, oldAddr) {
        this.formatAddressOnType();
      },

      skillName(selectedSkill) {
        if (!selectedSkill) {
          return;
        }

        let isAdded = false;
        for (let cap of this.capabilitiesParsed) {
          if (cap.ref === selectedSkill.ref) {
            isAdded = true;
            break;
          }
        }
        if (!isAdded) {
          this.$set(this.capabilitiesParsed, this.capabilitiesParsed.length - 1, selectedSkill);
        } else {
          notifyError(this, this.$i18n.t('message.agentsView.errors.skillAlreadAdded'));
        }
      }
    },

    methods: {
      checkSkillIntervals: function(intervals, number2Check) {
        if (!intervals || !intervals.length) {
          return false;
        }

        number2Check = parseFloat(number2Check);
        let high;
        let low;
        let match = false;
        for (let interval of intervals) {
          match = true;
          if (interval.low) {
            low = parseFloat(interval.low.boundary);
            if (interval.low.inclusive) {
              if (number2Check < low) { match = false }
            } else {
              if (number2Check <= low) { match = false }
            }
          }
          if (interval.high) {
            high = parseFloat(interval.high.boundary);
            if (interval.high.inclusive) {
              if (number2Check > high) { match = false }
            } else {
              if (number2Check >= high) { match = false }
            }
          }

          if (match) {
            return true;
          }
        }

        return false;
      },

      close() {
        this.$emit('close');
      },

      //TODO add it in an utility class
      getIntervalsStr: function(intervals) {
        let intervalString = '';
        let collection = [];
        for (let interval of intervals) {
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

      getTitle: function () {
        return this.agent != null ? this.$i18n.t('message.agentsView.editAgent') : this.$i18n.t('message.agentsView.createAgent')
      },

      increaseInitSkillFetched: function() {
        this.fetchedInitSkills++;
        if (this.fetchedInitSkills >= Object.keys(this.capabilities).length) {
          this.isFetchingInitSkills = false;
        }
      },

      loadSkills: _.debounce(async function () {
        this.isFetchingSkills = true;
        try {
          const data = await getSkills(this.routerRef);
          this.skills = data.data;
          this.isFetchingSkills = false;
        } catch (e) {
          this.isFetchingSkills = false;
          console.log(e);
        }
      }, 750),

      loadSkill: async function(skillRef, data) {
        try {
          let skill = await getSkill(this.agent.routerRef, skillRef);
          if (skill.domain.type === 'bool') {
            skill.data = !!data;
          } else {
            skill.data = Array.isArray(data) ? data : [data];
          }
          this.capabilitiesParsed.push(skill);
          this.increaseInitSkillFetched();
        } catch (e) {
          let skill = { ref: skillRef };
          skill.data = Array.isArray(data) ? data : [data];
          skill.multivalue = Array.isArray(data);
          this.capabilitiesParsed.push(skill);

          this.increaseInitSkillFetched();
        }
      },

      ok: function () {
        if (!this.validateFields()) {
          return;
        }

        fields.forEach(f => {
          if (this.newAgent[f] === this.agent[f]) {
            delete(this.newAgent[f]);
          }
        });

        Object.keys(this.newAgent)
          .filter(f => !fields.includes(f))
          .forEach(f => delete(this.newAgent[f]));

        this.newAgent.capabilities = this.getCapabilities();

        const agent = Object.assign({}, this.newAgent);
        if (agent.hasOwnProperty('address')) {
          const formattedPhone =
            this.formatPhoneNumber(agent.address, PhoneNumberFormat.RFC3966);
          if (formattedPhone) {
            agent.address = formattedPhone[1];
          }
        }

        if(this.isEdit) {
          this.$emit('update', agent);
        } else {
          this.$emit('add', agent);
        }
      },

      validateFields: function() {
        this.errors = { cap: []};
        for (let i = 0; i < this.capabilitiesParsed.length; i++) {
          const capability = this.capabilitiesParsed[i];
          if (!capability.domain) {
            continue;
          }
          if (capability.data == null || capability.data.length === 0) {
            this.errors.cap.index = i;
            this.errors.cap.text = this.$i18n.t('message.errors.emptyField');
          }

          switch (capability.domain.type) {
            case "string":
              for (let data of capability.data) {
                if (!data.match(new RegExp(capability.domain.regex))) {
                  this.errors.cap.push(this.$i18n.t('message.agentsView.errors.regexFail', [data, capability.ref]));
                }
              }
              break;
            case "number":
              let regexNumber = new RegExp('^[-+]*\\d+$');
              for (let data of capability.data) {
                if (!regexNumber.test(data)) {
                  this.errors.cap.push(this.$i18n.t('message.agentsView.errors.notNumber', [data]));
                }
                if (!this.checkSkillIntervals(capability.domain.intervals, data)) {
                  this.errors.cap.push(this.$i18n.t('message.agentsView.errors.failedSkillIntervals', [data, capability.ref]));
                }
              }
              break;
          }
        }

        return this.errors.cap.length === 0;
      },


      /*Address formatting
      ---------------------*/
      formatAddressOnType: _.debounce(
        function () {
          try {
            this.formatAddress();
          } catch (e) {
            // Ignore
          }
        },
        750
      ),

      formatAddress: function () {
        let address = this.newAgent.address;
        if (!address || address.startsWith("sip:")) {
          return;
        }
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

        const formattedPhone = this.formatPhoneNumber(address, PhoneNumberFormat.INTERNATIONAL);
        if (formattedPhone) {
          [country, formatted] = formattedPhone;
        }

        this.addressCountry = country;
        this.newAgent.address = formatted;
      },

      formatPhoneNumber: function (phoneNumber, phoneNumberFormat) {
        try {
          const parsedPhone = PhoneNumberUtil.parseAndKeepRawInput(phoneNumber);
          if (PhoneNumberUtil.isPossibleNumber(parsedPhone)
            && PhoneNumberUtil.isValidNumber(parsedPhone)) {
            const country = PhoneNumberUtil.getRegionCodeForNumber(parsedPhone);
            const formatted = PhoneNumberUtil.format(parsedPhone, phoneNumberFormat);
            return [country, formatted];
          }
        } catch (ignore) {
          // Nothing to do
        }
        return false;
      },


      /*Capabilities
      --------------*/
      addCapability: function() {
        this.isAddingCapability = true;
        this.capabilitiesParsed.push({});
      },

      addTag: function(cap) {
        if(!cap.data) {
          cap.data = [];
        }
        cap.data.push(this.capVal);
        this.capVal = null;
      },

      clearCapabilitiesErrors: function(index) {
        if (index == null || (this.errors.cap && this.errors.cap.index === index)) {
          this.$delete(this.errors, 'cap');
        }
      },

      deleteBtn: function () {
        if (this.isEdit) {
          this.$emit('delete', this.agent);
        }
      },

      editCapability(cap, index){
        this.isEditingCapability = true;
        this._originalCapability = cap;
        this.modifiedCapability = this.backup(cap);
        this.modifiedCapabilityIndex = index;
      },

      finishAddingCap(cap, index) {
        this.capabilitiesParsed[index] = cap;
        this.isAddingCapability = false;
      },

      finishEditingCap(cap, index, isCancelling) {
        if(isCancelling) {
          this.capabilitiesParsed[index] = this._originalCapability;
        } else {
          this.capabilitiesParsed[index] = this.modifiedCapability;
        }

        this._originalCapability = undefined;
        this.isEditingCapability = false;
        this.modifiedCapability = undefined;
        this.modifiedCapabilityIndex = undefined;
      },

      getCapabilities: function() {
        let capabilities = {};
        for (let capability of this.capabilitiesParsed) {
          if (!capability.ref) {
            continue;
          }

          let data = capability.data;
          if (!capability.multivalue && data && Array.isArray(data) && data.length > 0) {
            data = data[0];
          }
          capabilities[capability.ref] = data;
        }

        return capabilities;
      },

      parseCapabilities: function(capabilities) {
        if (!capabilities) {
          return;
        }

        this.isFetchingInitSkills = true;
        let hasCaps = false;
        for (let capability in capabilities) {
          this.loadSkill(capability, capabilities[capability]);
          hasCaps = true;
        }
        if (!hasCaps) {
          this.isFetchingInitSkills = false;
        }
      },

      removeCapability: function(index) {
        if(this.isAddingCapability && this.capabilitiesParsed.length === index + 1) this.isAddingCapability = false;

        this.capabilitiesParsed.splice(index, 1);

        if (this.errors && this.errors.cap && this.errors.cap.index != null) {
          if (this.errors.cap.index === index) {
            this.clearCapabilitiesErrors();
          } else if (this.errors.cap.index > index) {
            this.errors.cap.index--;
          }
        }
      },

      removeTag: function(cap, tag) {
        cap.data = _.pull(cap.data, tag);
        // cap.data.splice(index, 1);
      }
    }
  }
</script>

<style lang="scss" scoped>
  @import "~assets/volta/scss/lib/_variables.scss";

  .Vlt-badge--dismissed {
    display: none;
  }

  .Cr-agent__actions {
    margin-top: 27px;
  }

  .Cr-agent__add-skill {
    display: block;
    margin-top: 1em;
  }

  .Cr-agent__skill {
    align-items: baseline;
    border-bottom: 1px solid $grey-light;
  }

  .Cr-agent__skills .Vlt-badge {
    margin-bottom: 5px;
  }

  .flag--empty {
    background: #e7ebee;
    display: block;
    height: 1em;
    margin-top: 3px;
    width: 1.33333333em;
  }
</style>
