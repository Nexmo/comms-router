<template>
  <vlt-field>
    <div class="Vlt-composite">
      <div class="Vlt-composite__prepend Vlt-composite__prepend--icon">
          <vlt-icon no-class icon="search" />
      </div>
      <div class="Vlt-input">
        <input type="search" v-on="inputListeners" :placeholder="$t('message.search')" />
      </div>
    </div>
  </vlt-field>
</template>

<script>
  import { VltField, VltIcon } from '../assets/volta/vue';

  export default {
    name: 'cr-search',

    components: {
      VltField,
      VltIcon
    },

    computed: {
        inputListeners: function() {
          let vm = this

          return Object.assign({}, this.$listeners, {
            input: function(event) {  
              vm.$emit('input', event.target.value)
            }
          })
        }
      }
    }
</script>
