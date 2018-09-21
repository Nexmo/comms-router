<template>
  <svg @click="handleClick" :class="{'Vlt-icon': !noClass,'pointer': pointer, 'spin': spin, 'Vlt-icon--small': small}">
    <use :xlink:href="icon_path" />
  </svg>
</template>

<script>
  // Need to import icon here
  // import './folder/icon.svg';
  import VltIcons from '../volta-icons.svg'

  export default {
    component: {
      VltIcons
    },

    name: 'vlt-icon',

    methods: {
      handleClick() {
        if (this.clickable) {
          this.$emit('click');
        }
      }
    },

    computed: {
      icon_path: function () {
        return `${VltIcons}#Vlt-icon-${this.icon}`;
      }
    },

    props: {
      icon: {
        type: String,
        default: ''
      },
      clickable: {
        type: Boolean,
        default: true
      },
      noClass: {
        type: Boolean,
        default: false
      },
      pointer: {
        type: Boolean,
        default: false
      },
      spin: {
        type: Boolean,
        default: false
      },
      small: {
        type: Boolean,
        default: false
      }
    }
  };
</script>

<style scoped>
  .Vlt-icon.pointer {
    cursor: pointer;
  }
  .spin {
    animation: spin 1s infinite linear reverse;
  }
</style>
