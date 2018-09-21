
import Vue from 'vue'
import axios from 'axios'
import VueAxios from 'vue-axios'
import NProgress from 'vue-nprogress'
import { sync } from 'vuex-router-sync'
import App from './App.vue'
import router from './router'
import store from './store'
import * as filters from './filters'
import { TOGGLE_SIDEBAR, LOGGED_IN } from 'vuex-store/mutation-types'
import VueGoodTable from 'vue-good-table'
import FlagIcon from 'vue-flag-icon'
import Buefy from 'buefy'
import VueI18n from 'vue-i18n'
import messages from './i18n'
import {isLoggedIn} from "./api/auth";
import mixins from "./mixins";

require ('./custom-directives.js');

Vue.router = router;
Vue.use(VueGoodTable);
Vue.use(VueAxios, axios);
Vue.use(NProgress);

Vue.use(FlagIcon);
Vue.use(Buefy, {
  defaultIconPack: 'fa'
});
Vue.use(VueI18n);

//mixins
Vue.mixin({methods: mixins});

// Enable devtools
Vue.config.devtools = true;

sync(store, router);

const nprogress = new NProgress({ parent: '.nprogress-container' });

const { state } = store;

if (isLoggedIn()) {
  store.commit(LOGGED_IN, true);
}

router.beforeEach((route, redirect, next) => {
  if (!state.app.auth.loggedIn && route.path !== '/') {
    next('/');
    return;
  }

  if (state.app.device.isMobile && state.app.sidebar.opened) {
    store.commit(TOGGLE_SIDEBAR, false)
  }
  next();
});

Object.keys(filters).forEach(key => {
  Vue.filter(key, filters[key])
});

const i18n = new VueI18n({
  locale: 'en', // set locale
  dateTimeFormats: messages,
  messages // set locale messages
});

const app = new Vue({
  router,
  store,
  nprogress,
  i18n,
  ...App
});

export { app, router, store }
