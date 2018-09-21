import Vue from 'vue'

// Register a global custom directive called `v-focus`
Vue.directive('focus', {
  // When the bound element is inserted into the DOM...
  inserted: function (el) {
    // Focus the element
    if (el.tagName.toLowerCase() === 'input' || el.getAttribute("tabindex") === '0') {
      el.focus()
    } else {
      let input = el.querySelector("[tabindex='0']");
      if (!input) {
        input = el.getElementsByTagName('input')[0];
      }
      if (input) {
        setTimeout(() => input.focus(), 10);
      }
    }
  }
});
