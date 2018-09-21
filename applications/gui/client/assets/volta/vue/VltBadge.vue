<template>
  <span v-if="!dismissed" class="Vlt-badge" :class="classObject">
    <slot></slot>
    <button v-if="dismissable" @click="dismiss($event)" class="Vlt-badge__dismiss"></button>
  </span>
</template>

<script>
  import Vue from 'vue';

  export default {
    name: "vlt-badge",

    props: {
      color: String,
      dismissable: Boolean,
      large: Boolean,
      stacked: {
        type: Boolean,
        default: false
      },
      small: Boolean
    },

    data: function() {
      return {
        dismissed: false
      }
    },

    computed: {
      classObject: function() {
        let badgeColor;
        let obj = {
          'Vlt-badge--dismissable': this.dismissable,
          'Vlt-badge--large': this.large,
          'Vlt-badge--stacked': this.stacked
        }

        if(this.color) {
          badgeColor = `Vlt-badge--${this.color}`;
          obj[badgeColor] = true;
        }
        return obj;
      }
    },

    methods: {
      dismiss (event) {
        event.stopPropagation();
        event.preventDefault();

        this.dismissed = true; 
        this.$emit('dismissed');
      }
    }
  }
</script>

<style lang="scss" scoped>
  @import "../scss/lib/_variables.scss";

  .Vlt-badge--stacked {
    margin-bottom: $unit1;
  }
</style>