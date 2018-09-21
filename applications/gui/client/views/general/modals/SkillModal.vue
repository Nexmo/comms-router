<template>
  <vlt-modal :title="getTitle()" :visible="visible" :disable-ok="isSaveDisabled" large
              :extra-btn-label="isEdit ? $t('message.skillsView.delete.title'): null" extra-btn-class="Vlt-btn--destructive"
              extra-btn-icon="bin" @extraBtnClick="deleteBtn" role="dialog" @confirm="ok" @close="cancel" :okText="isEdit ? $t('message.save') : $t('message.create')">
      
      <vlt-field :label="$t('message.name')" :error-message="errors.name">
        <vlt-input :disabled="isEdit" id="nameinput" type="text" v-model="name" :val="name"></vlt-input>
      </vlt-field>

      <vlt-field :label="$t('message.description')" optional>
        <div class="Vlt-textarea">
          <textarea id="descr" v-model="description" maxlength="200"></textarea>
        </div>
      </vlt-field>

      <vlt-field :label="$t('message.type')" :error-message="errors.type">
        <vlt-radio :label="$t('message.skillsView.typeEnum')" name="type" val="enumeration" :checked="type === 'enumeration'" v-model="type" :hint="$t('message.skillsView.tooltip.enumeration')"></vlt-radio>
        <vlt-radio :label="$t('message.skillsView.typeNumber')" name="type" val="number" :checked="type === 'number'" v-model="type" :hint="$t('message.skillsView.tooltip.number')"></vlt-radio>
        <vlt-radio :label="$t('message.skillsView.typeText')" name="type" val="string" :checked="type === 'string'" v-model="type" :hint="$t('message.skillsView.tooltip.text')"></vlt-radio>
      </vlt-field>

      <vlt-field v-if="type === 'enumeration'" :label="$t('message.skillsView.acceptableValues')">
        <vlt-taginput id="valueinput1" v-model="values" />
      </vlt-field>

      <div v-else-if="type === 'number'">
        <label class="Vlt-label">{{$t('message.skillsView.range')}} <small class="Vlt-form__element_optional">{{$t('message.optional_')}}</small></label>

        <div class="Vlt-form__group" v-for="(interval, index) in intervals" :key="index">
          <vlt-field :label="$t('message.skillsView.startRange')">
            <vlt-input type="number" :val="interval.low.boundary" v-model="interval.low.boundary" step="0.1"></vlt-input>
          </vlt-field>

          <vlt-field :label="$t('message.skillsView.endRange')">
            <vlt-input type="number" :val="interval.high.boundary" v-model="interval.high.boundary" step="0.1"></vlt-input>
          </vlt-field>
          
          <button v-if="intervals.length > 1" @click='removeInterval(index)' :aria-label="$t('message.skillsView.removeInterval')" class="Vlt-btn Vlt-btn--icon Vlt-btn--app Vlt-btn--destructive">
            <svg class="Vlt-icon"><use xlink:href="#Vlt-icon-bin"></use></svg>
          </button>
        </div>

        <div>
          <a class="allowed-ranges__add" @click='addInterval' :aria-label="$t('message.skillsView.addInterval')">
            <span>{{ $t('message.skillsView.addInterval') }}</span>
          </a>
        </div>
      </div>

      <vlt-field v-else-if="type === 'string'" :label="$t('message.skillsView.filter')" optional>
        <vlt-input type="text" :val="regex" v-model="regex" id="valueinput" :hint="$t('message.skillsView.filterDescr')"></vlt-input>
      </vlt-field>      

      <div v-if="type !== 'bool'" class="Vlt-grid Vlt-grid--margin-top2">
        <div class="Vlt-col">
          <vlt-field> 
            <vlt-checkbox v-model="multiple" :checked="multiple" :label="$t('message.skillsView.allowMultipleValues')" :tooltip-title="$t('message.skillsView.tooltip.multipleValues')"></vlt-checkbox>
          </vlt-field>
        </div>
      </div>
  </vlt-modal>
</template>

<script>
  import { VltCheckbox, VltField, VltIcon, VltInput, VltModal, VltRadio, VltTaginput } from '../../../assets/volta/vue';

  export default {
    name: "skill-modal",
    components: {
      VltCheckbox,
      VltField,
      VltIcon,
      VltInput,
      VltModal,
      VltRadio,
      VltTaginput
    },

    data() {
      return {
        name: '',
        description: '',
        type: null,
        regex: null,
        multiple: false,
        values: [],
        intervals: [ ],
        errors: {
          intervals: []
        },
        isEdit: false
      }
    },

    props: {
      visible: Boolean,
      skill: Object
    },

    methods: {
      getTitle: function () {
        return this.skill != null ? this.$i18n.t('message.skillsView.editSkillTitle') : this.$i18n.t('message.skillsView.newSkillTitle')
      },

      cancel: function() {
        this.$emit('close');
      },

      ok: function () {
        if (!this.validateFields()) {
          return
        }

        let skill = {
          ref: this.name,
          description: this.description,
          multivalue: this.multiple,
          domain: {
            type: this.type,
          }
        };

        switch (this.type) {
          case 'string':
            skill.domain.regex = this.regex;
            break;
          case 'enumeration':
            skill.domain.values = this.values;
            break;
          case 'number':
            for (let interval of this.intervals) {
              if (interval.low.boundary === Infinity || interval.low.boundary === -Infinity) {
                interval.low.boundary = interval.low.boundary.toString();
                interval.low.inclusive = false;
              } else {
                interval.low.inclusive = true;
              }
              if (interval.high.boundary === Infinity || interval.high.boundary === -Infinity) {
                interval.high.boundary = interval.high.boundary.toString();
                interval.high.inclusive = false;
              } else {
                interval.high.inclusive = true;
              }
            }
            skill.domain.intervals = this.intervals;
            break;
          case 'bool':
            delete skill.multivalue;
            break;
        }
        if (this.type == null) {
          delete skill.domain;
          delete skill.multivalue;
        }

        if(this.isEdit) {
          this.$emit('update', skill);
        } else {
          this.$emit('add', skill);
        }
      },

      validateFields: function() {
        let hasErrors = false;

        this.$delete(this.errors, 'name');

        if (!this.name) {
          this.$set(this.errors, 'name', this.$t('message.errors.emptyField'));
          hasErrors = true;
        } else if (/["'();,=!~<> ]/.test(this.name)) {
          //'"' | "'" | "(" | ")" | ";" | "," | "=" | "!" | "~" | "<" | ">";
          this.$set(this.errors, 'name', this.$t('message.skillsView.nameMessage'));
          hasErrors = true;
        }

        this.$delete(this.errors, 'type');
        if (!this.type) {
          this.$set(this.errors, 'type', this.$t('message.errors.emptyField'));
          hasErrors = true;
        }

        hasErrors = hasErrors || !this.validateIntervals()

        return !hasErrors;
      },

      deleteBtn: function () {
        if (this.isEdit) {
          this.$emit('delete', this.skill);
        }
      },

      addInterval: function() {
        this.intervals.push({
          low: {},
          high: {}
        });
      },

      init: function () {
        this.addInterval();
        
        if (this.skill) {
          this.name = this.skill.ref;
          this.description = this.skill.description;
          this.multiple = this.skill.multivalue;
          this.type = this.skill.domain.type;

          switch (this.type) {
            case 'string':
              this.regex = this.skill.domain.regex;
              break;
            case 'enumeration':
              this.values = this.skill.domain.values.slice();
              break;
            case 'number':
              this.intervals = this.skill.domain.intervals.slice();
              this.intervals.forEach(interval => {
                if (!interval.low) {
                  interval.low = {boundary: -Infinity, inclusive: false};
                }
                if (!interval.high) {
                  interval.high = {boundary: Infinity, inclusive: false};
                }
              });

              break;
          }
        }
      },

      removeInterval: function(index) {
        delete this.errors.intervals[index];

        this.intervals.splice(index,  1);
      },

      validateIntervals: function() {
        this.errors.intervals = [];
        let hasError = false;

        let interval;
        let high;
        let low;
        for (let i = 0; i < this.intervals.length; i++) {
          interval = this.intervals[i];
          try {
            high = parseFloat(interval.high.boundary);
            low = parseFloat(interval.low.boundary);
          } catch (e) {
            this.errors.intervals[i] = this.$t('message.skillsView.errors.invalidNumber');
            hasError = true;
            continue;
          }

          // if (!high && !low && this.intervals.length > 1) {
          //   this.errors.intervals[i] = this.$t('message.skillsView.errors.emptyInterval');
          //   hasError = true;
          //   continue;
          // } else
          if (high < low) {
            this.errors.intervals[i] = this.$t('message.skillsView.errors.maxLowerMin');
            hasError = true;
            continue;
          } else {
            if (!high) {
              high = Infinity;
            }
            interval.high.boundary = high;
            if (!low) {
              low = -Infinity;
            }
            interval.low.boundary = low;
            //check for overlapping
            let prevInterval;
            let prevLow;
            let prevHigh;
            for (let j = 0; j < i; j++) {
              prevInterval = this.intervals[j];
              prevLow = parseFloat(prevInterval.low.boundary);
              prevHigh = parseFloat(prevInterval.high.boundary);
              if ((high <= prevHigh && high > prevLow) || (low >= prevLow && low < prevHigh)
                  || (high === prevHigh && low === prevLow) || (low < prevLow && high > prevHigh)) {
                this.errors.intervals[i] = this.$t('message.skillsView.errors.alreadyIncludedInterval', [prevLow, prevHigh]);
                hasError = true;
                break;
              }
            }
          }
        }

        return !hasError;
      }
    },

    watch: {
      visible: function(value) {
        if(value) {
          this.init();
        }
      }
    },

    computed: {
      isSaveDisabled: function() {
        // if (!this.name || !this.type) {
        //   return true;
        // }
        return false;
      }
    },

    mounted() {
      if(this.skill) {
        this.isEdit = true;
      }
      this.init();
    },

  }
</script>

<style lang="scss" scoped>
  @import "~assets/volta/scss/lib/_variables.scss";

  .Vlt-form__element_optional {
    color: $grey-darker;
  }

  .allowed-ranges__add {
    color: $blue-dark;
  }

</style>
