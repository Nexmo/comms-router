<template>
  <div>
    <div class="Vlt-grid">
      <div class="Vlt-col">
        <div v-if="!capability.domain">
          <p v-if="capability.data" class="Vlt-form__element__error">{{$t('message.agentsView.errors.unknownSkill', [capability.ref])}}</p>
        </div>

        <div v-if="capability.domain && capability.domain.type === 'enumeration'">
          <div class="Vlt-grid Vlt-grid--narrow">
            <div class="Vlt-col">
              <vlt-dropdown v-model="capVal" showSelection :options="capability.domain.values" />
            </div>
            <div class="Vlt-col">
              <button v-if="capability.multivalue" @click='addTag()' class="Vlt-btn Vlt-btn--app Vlt-btn--small Vlt-btn--icon Vlt-btn--tertiary">
                <vlt-icon icon="plus" />
              </button>
            </div>
          </div>
        </div>

        <div v-else-if="capability.domain && capability.domain.type === 'string'">
          <vlt-field :label="$t('message.agentsView.enterRegex', [capability.domain.regex])">

            <div v-if="capability.multivalue" class="Vlt-composite">
              <vlt-input :val="capVal" v-model="capVal"/>
              <div  class="Vlt-composite__append">
                <button @click='addTag(capability)' class="Vlt-btn Vlt-btn--app Vlt-btn--small Vlt-btn--icon Vlt-btn--tertiary">
                  <vlt-icon icon="plus" />
                </button>
              </div>
            </div>

            <vlt-input v-else :val="capVal" v-model="capVal"/>
          </vlt-field>
        </div>

        <div v-else-if="capability.domain && capability.domain.type === 'number'">
          <vlt-field :label="$t('message.addValue')">

            <div v-if="capability.multivalue" class="Vlt-composite">
              <vlt-input type="number" :val="capVal" v-model="capVal"/>
              <div class="Vlt-composite__append">
                <button class="Vlt-btn Vlt-btn--app Vlt-btn--small Vlt-btn--icon Vlt-btn--tertiary" @click='addTag(capability)'>
                  <vlt-icon icon="plus" />
                </button>
              </div>
            </div>

            <vlt-input v-else :val="capVal" v-model="capVal" type="number"/>
          </vlt-field>
        </div>
      </div>

      <div class="Vlt-col Vlt-col--1of4 Vlt-col--right Cr-agent__actions">
        <button @click='cancel' class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--small Vlt-btn--tertiary">
          <vlt-icon icon="cross" />
        </button>
        <button v-if="capability.domain" @click='finishAddingCapability' class="Vlt-btn Vlt-btn--app Vlt-btn--icon Vlt-btn--small Vlt-btn--tertiary">
          <vlt-icon icon="check" />
        </button>
      </div>
    </div>

    <div v-if="capability.multivalue">
      <vlt-badge v-for="tag in capability.data" :key="tag">{{tag}}</vlt-badge>
    </div>
  </div>
</template>

<script>
  import { VltField, VltBadge, VltDropdown, VltInput, VltIcon } from '../assets/volta/vue';

  export default {
    components: {
      VltBadge,
      VltDropdown,
      VltField,
      VltInput,
      VltIcon
    },

    props: {
      capability: Object,
      index: Number,
      isEdit: Boolean
    },

    data() {
      return {
        capVal: undefined,
      }
    },

    methods: {
      cancel: function() {
        if(this.isEdit) {
          this.$emit('done', this.capability, this.index, true);
        } else {
          this.$emit('removeCapability', this.index);
        }
      },

      addTag: function() {
        if(!this.capability.data) {
          this.capability.data = [];
        }
        this.capability.data.push(this.capVal);
        this.capVal = null;
      },

      finishAddingCapability() {
        if(this.capVal)  {
          this.addTag(this.capability);
        }
        this.$emit('done', this.capability, this.index);
      }
    }
  }
</script>
