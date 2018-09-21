import _ from 'lodash';
import Vue from 'vue';
import VltModal from '../assets/volta/vue/VltModal.vue';

function notifyError(self, error, title) {
  let errorMsg = error && error.response && error.response.data && error.response.data.error
                  ? `${error.response.data.error.code}: ${error.response.data.error.description}` : '';
  if (!errorMsg) {
    if (typeof error === 'string') {
      errorMsg = error;
    } else {
      [errorMsg, title] = [title, ''];
    }
  }

  const propsData = {
    visible: true,
    title: title ? title : self.$i18n.t('message.error'),
    message: errorMsg,
    canCancel: false,
    dynamic: true,
    okText: "Ok"
  }

  const ModalComponent = Vue.extend(VltModal)
  return new ModalComponent({
      el: document.createElement('div'),
      propsData,
      methods: {
        confirm: function(){
            this.visible = false
        }
      }
  });
}

function confirmDelete(self, title, message, confirmFn) {
   const propsData = {
    cancelText: self.$i18n.t(`message.cancel`),
    confirmBtnIcon: 'bin',
    destructive: true,
    dynamic: true,
    message: message,
    okText: self.$i18n.t(`message.delete`),
    title: title,
    visible: true
  }

  const methods = {
    close: function() {
      this.visible = false;
    },
    confirm: function() {
      confirmFn();
      this.visible = false;
    }
  }

  const ModalComponent = Vue.extend(VltModal)
  return new ModalComponent({
      el: document.createElement('div'),
      propsData,
      methods
  });
}

function hasClass(element, className) {
  if(!element) {
    return false;
  }

  return (` ${element.className} `).replace(/[\n\t]/g, " ").indexOf(` ${className} `) > -1;
}

export { confirmDelete, hasClass, notifyError };
