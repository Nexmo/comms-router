<template>
  <div class="Vlt-radio" :class="{ 'Vlt-radio--inline': inline }">
    <label>
      <span class="Vlt-radio__button">
        <input type="radio" :value="val" :name="name" :checked="checked" v-on="inputListeners"/>
        <span class="Vlt-radio__icon"></span>
      </span>
      <span>
        {{label}}
        <small v-if="hint" class="Vlt-radio__hint">{{hint}}</small>
      </span>
    </label>
  </div>
</template>

<script>
    export default {
      name: "vlt-radio",

      props: {
        checked: Boolean,
        hint: String,
        inline: {
          type: Boolean,
          default: false
        },
        label: String,
        name: String,
        //need to use 'val' so we can emit the value to the parent, value does not work with v-model
        val: String
      },

      computed: {
        inputListeners: function() {
          let vm = this;

          return Object.assign({}, this.$listeners, {
            input: function(event) {  
              vm.$emit('input', event.target.value)
            }
          })
        }
      }
    }
</script>

<style lang="scss" scoped>
  @import "../scss/lib/_variables.scss";

  .Vlt-radio__hint {
    color: $grey-darker;
    display: flex;
  }

</style>