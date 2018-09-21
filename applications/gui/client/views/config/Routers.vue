<template>
  <section>
    <h1 class="Vlt-title--icon">
      <vlt-icon icon="flash-2" class="Vlt-green"/>
      {{$t('message.routers')}}
    </h1>
    <vlt-dropdown :label="$t('message.router')"
                  showSelection
                  :options="routers"
                  property="ref"
                  v-model="selectedRouter"
                  :selected="selectedRouter"></vlt-dropdown>
  </section>
</template>

<script>
  import {mapGetters} from 'vuex';
  import {getRouters} from '../../api/api-client';
  import {VltDropdown, VltField, VltIcon} from '../../assets/volta/vue';

  export default {
    components: {
      VltDropdown,
      VltField,
      VltIcon
    },

    data() {
      return {
        selectedRouter: undefined,
        routers: []
      }
    },

    computed: {
      ...mapGetters({
        router: 'routerRef'
      })
    },

    watch: {
      selectedRouter(selected) {
        this.setRouter(selected);
      }
    },

    methods: {
      retrieveRouters() {
        return getRouters()
        .then((response) => {
          this.routers = response.data;
        });
      },

      setRouter(ref) {
        //...mapActions not working with ...mapGetters
        if (ref) {
          this.$store.dispatch('setRouterRef', ref);
        }
      }
    },

    beforeMount() {
      this.retrieveRouters();
    },

    mounted() {
      this.selectedRouter = this.router;
    }

  }
</script>

<style lang="scss" scoped>
</style>
