<template>
  <span class="Vlt-tooltip">
    <vlt-icon class="Vlt-blue" icon="help-negative" small></vlt-icon>
  </span>
</template>
<script>
  import Tooltip from 'tooltip.js';
  import VltIcon from './VltIcon';

    export default {
      name: "vlt-tooltip",

      props: {
        position: String,
        title: String
      },

      components:{
        VltIcon
      },

      mounted() {
        return new Tooltip(this.$el, {
          html: true,
          template: `<div class="Vlt-tooltip--js" role="tooltip">
                      <div class="tooltip-arrow Vlt-tooltip__arrow"></div>
                      <div class="tooltip-inner Vlt-tooltip__content"></div>
                    </div>`, 
          title: this.title,
          placement: this.position || "left"
        });
      }
    }
</script>