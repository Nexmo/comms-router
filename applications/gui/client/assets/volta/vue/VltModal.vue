<template functional>
  <!--Note: maybe there will be cases is which we want the state of the modal to be remembered on close, if that happens we can move v-if and put it instead on parent elements-->
  <div class="Vlt-modal" v-if="visible" :id="id" :class="{'Vlt-modal_visible': visible, 'Vlt-modal--large': large, 'Vlt-modal--message': !!message}">
    <div class="Vlt-modal__panel" @keyup.enter="confirm" @keyup.escape="close">
      <header class="Vlt-modal__header">
        <h4>{{ title }}</h4>
        <button class="Vlt-modal__dismiss" aria-label="Close" @click="close"></button>
      </header>
      <section class="Vlt-modal__content">
        <slot><p>{{message}}</p></slot>
      </section>
      <footer class="Vlt-modal__footer">
          <button v-if="extraBtnLabel" class="Vlt-btn Vlt-btn--app" :class="extraBtnClass" @click="extraBtnClick">
            <vlt-icon v-if="extraBtnIcon" :icon="extraBtnIcon"></vlt-icon>
            <span>{{extraBtnLabel}}</span>
          </button>
          <button v-if="canCancel" class="Vlt-btn Vlt-btn--app Vlt-btn--tertiary" @click="close">{{getCancelText()}}</button>
          <button class="Vlt-btn Vlt-btn--app" :class="getPrimaryBtnClass()" @click="confirm">
            <vlt-icon v-if="confirmBtnIcon" :icon="confirmBtnIcon"></vlt-icon>
            {{getOkText()}}
          </button>
      </footer>
    </div>
  </div>
</template>

<script>
  import VltIcon from './VltIcon'

  export default {
    name: "vlt-input",

    components: {
      VltIcon
    },

    props: {
      canCancel: {
        default: true,
        type: Boolean
      },
      cancelText: String,
      confirmBtnIcon: {
        type: String,
        required: false
      },
      destructive: {
        default: false,
        type: Boolean
      },
      dynamic: {
        default: false,
        type: Boolean
      },
      extraBtnClass: String,
      extraBtnIcon: String,
      extraBtnLabel: String,
      id: String,
      large: Boolean,
      message: String,
      notify: {
        default: false,
        type: Boolean
      },
      okText: String,
      title: String,
      visible: Boolean
    },

    methods: {
      close () {
        this.$emit('close');
      },

      confirm () {
        this.$emit('confirm');
      },

      extraBtnClick () {
        this.$emit('extraBtnClick');
      },

      getCancelText () {
        return this.canCancel ? this.cancelText ? this.cancelText : 'Cancel' : null;
      },

      getOkText () {
        return this.okText ? this.okText : 'Ok';
      },

      getPrimaryBtnClass() {
        return this.destructive ? 'Vlt-btn--destructive' : 'Vlt-btn--secondary';
      }
    },

    beforeMount() {
      if(this.dynamic) {
        document.body.appendChild(this.$el)
      }         
    }
  }
</script>