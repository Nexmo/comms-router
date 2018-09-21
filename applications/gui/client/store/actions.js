import * as types from './mutation-types';

export const toggleSidebar = ({ commit }, config) => {
    if (config instanceof Object) {
        commit(types.TOGGLE_SIDEBAR, config)
    }
};

export const toggleDevice = ({ commit }, device) => commit(types.TOGGLE_DEVICE, device)

export const switchEffect = ({ commit }, effectItem) => {
    if (effectItem) {
        commit(types.SWITCH_EFFECT, effectItem)
    }
};

export const setAgentsRefreshInt = ({ commit }, interval) => {
    commit(types.REFRESH_INTERVAL_AGENTS, interval)
};

export const setQueuesRefreshInt = ({ commit }, interval) => {
    commit(types.REFRESH_INTERVAL_QUEUES, interval)
};

export const setAuthLoggedIn = ({ commit }, loggedIn) => {
  commit(types.LOGGED_IN, loggedIn)
};

export const setRouterRef = ({ commit }, routerRef) => {
  commit(types.ROUTER_REF, routerRef)
};
