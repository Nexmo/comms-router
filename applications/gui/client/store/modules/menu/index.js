import * as types from '../../mutation-types'
import lazyLoading from './lazyLoading'

// show: meta.label -> name
// name: component name
// meta.label: display label

const state = {
  items: [
    {
      name: 'monitor.agents',
      path: '/:routerRef/monitor/agents',
      meta: {
        label: 'message.agents',
        icon: 'receptionist',
        link: '/monitor/Agents.vue'
      },
      component: lazyLoading('monitor/Agents')
    },
    {
      name: 'monitor.queues',
      path: '/:routerRef/monitor/queues',
      meta: {
        label: 'message.queues',
        icon: 'stack',
        link: '/monitor/Queues.vue'
      },
      component: lazyLoading('monitor/Queues')
    }
  ],
  itemsGeneral: [
    {
      name: 'general.skills',
      path: '/:routerRef/skills',
      meta: {
        label: 'message.skills',
        icon: 'mind-map',
        link: '/general/Skills.vue'
      },
      component: lazyLoading('general/Skills')
    },
    {
      name: 'general.agents',
      path: '/:routerRef/agents',
      meta: {
        label: 'message.agents',
        icon: 'receptionist',
        link: '/general/Agents.vue'
      },
      component: lazyLoading('general/Agents')
    },
    {
      name: 'general.queues',
      path: '/:routerRef/queues',
      meta: {
        label: 'message.queues',
        icon: 'stack',
        link: '/general/Queues.vue'
      },
      component: lazyLoading('general/Queues')
    },
    {
      name: 'general.flows',
      path: '/:routerRef/flows/:ref?',
      meta: {
        label: 'message.flows',
        icon: 'flow',
        link: '/general/Flows.vue'
      },
      component: lazyLoading('general/Flows')
    }
  ]
};

export default {
  state
}
