import * as types from '../mutation-types'

const state = {
  device: {
    isMobile: false,
    isTablet: false
  },
  sidebar: {
    opened: false,
    hidden: false
  },
  effect: {
    translate3d: true
  },
  monitoring: {
    refreshIntervalAgents: 15,
    refreshIntervalQueues: 15
  },

  auth: {
    loggedIn: false
  },

  routerRef: ''
};

const mutations = {
  [types.TOGGLE_DEVICE] (state, device) {
    state.device.isMobile = device === 'mobile'
    state.device.isTablet = device === 'tablet'
  },

  [types.TOGGLE_SIDEBAR] (state, config) {
    if (state.device.isMobile && config.hasOwnProperty('opened')) {
      state.sidebar.opened = config.opened
    } else {
      state.sidebar.opened = true
    }

    if (config.hasOwnProperty('hidden')) {
      state.sidebar.hidden = config.hidden
    }
  },

  [types.SWITCH_EFFECT] (state, effectItem) {
    for (let name in effectItem) {
      state.effect[name] = effectItem[name]
    }
  },

  [types.REFRESH_INTERVAL_AGENTS] (state, interval) {
    state.monitoring.refreshIntervalAgents = interval;
  },

  [types.REFRESH_INTERVAL_QUEUES] (state, interval) {
    state.monitoring.refreshIntervalQueues = interval;
  },

  [types.LOGGED_IN] (state, isLogged) {
    state.auth.loggedIn = isLogged;
  },

  [types.ROUTER_REF] (state, routerRef) {
    state.routerRef = routerRef;
  }
};

export default {
  state,
  mutations
}
