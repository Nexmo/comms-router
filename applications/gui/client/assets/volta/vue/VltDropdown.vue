<template>
   <div class="Vlt-dropdown" :class="{ 'Vlt-dropdown--expanded' : expanded }">
    <button class="Vlt-dropdown__btn" @click="toggleDropdown($event)">
      <span v-if="label && (!hideLabel || !selectedOption)">{{label}}<span v-if="showSelection">:</span></span>
      <span v-if="showSelection" class="Vlt-dropdown__selection">
        {{ property && selectedOption ? selectedOption[property] : selectedOption }}
      </span>
    </button>
    <div class="Vlt-dropdown__panel">
      <div class="Vlt-dropdown__panel__content">
        <ul>
          <li v-for="option in options">
            <a class="Vlt-dropdown__link" @click="selectOption(option)">
              <span class="Vlt-dropdown__label">{{property ? option[property] : option}}</span>
            </a>
          </li>
        </ul>
      </div>
    </div>
  </div>
</template>

<script>
    export default {
      name: "vlt-dropdown",

      props: {
        hideLabel: {
          type: Boolean,
          default: false
        },
        label: String,
        options: Array,
        property: String,
        selected: {
          type: Object | String,
          required: false
        },
        showSelection: Boolean
      },

      data() {
        return {
          expanded: false,
          selectedOption: this.selected
        }
      },

      methods: {
        bodyListener(event) {

          this.expanded = false;
          document.removeEventListener('click', this.bodyListener);
        },

        toggleDropdown(event) {
          event.stopPropagation();
          this.expanded = !this.expanded;

          if(this.expanded) {
            document.addEventListener('click', this.bodyListener);

            this.$nextTick(() => {
              this.$el.querySelector('.Vlt-dropdown__panel__content').scrollIntoView();
            });
          }
        },

        selectOption(option) {
          let selection = option;

          this.selectedOption = selection;
          this.$emit('input', selection);
          this.expanded = false;
          document.removeEventListener('click', this.bodyListener);
        }
      },

      mounted() {
        if(!this.selected) {
          this.selectedOption = this.options[0];
          this.$emit('input', this.selectedOption);
        }
      },

      watch: {
        selected(selected) {
          this.selectedOption = selected;
        }
      }
    }
</script>
