<template>
  <nav v-if="loggedIn" class="Vlt-sidenav Vlt-sidenav--dark" :class="{'Vlt-sidenav_visible' : sidebar.opened}">
    <div class="Vlt-sidenav__block Vlt-sidenav__block--logo Vlt-M-plus">
      <a class="Vlt-sidenav__logo" href="/">
        <img src="~assets/vg-logo.svg">
      </a>
    </div>

    <div class="Vlt-sidenav__block Vlt-sidenav__block--link">
      <ul class="Vlt-sidemenu Vlt-sidenav__elem--full">
        <li>
          <span class="Vlt-sidemenu__link">
            <vlt-icon icon="profile" small class="Vlt-green"/>
            <span class="Vlt-sidemenu__label">
              {{ name }} ({{role}})
            </span>
          </span>
        </li>
        <li>
          <router-link to="/routers" class="Vlt-sidemenu__link" active-class="Vlt-sidemenu__link_active">
            <vlt-icon icon="flash-2" small class="Vlt-green"/>
            <span class="Vlt-sidemenu__label">
              {{ $t("message.router") }}: {{router.ref}}
            </span>
          </router-link>
        </li>
      </ul>
    </div>

    <div class="Vlt-sidenav__scroll">
      <ul class="Vlt-sidemenu">
        <li role="separator"><h5 class="Vlt-sidemenu__title">{{ $t("message.configuration") }}</h5></li>
        <li v-for="(item, index) in menuGeneral">
          <router-link :to="{ name: item.name, params: { routerRef: router.ref } }" class="Vlt-sidemenu__link" active-class="Vlt-sidemenu__link_active"> <!-- :exact="true" -->
            <vlt-icon :icon="item.meta.icon" small />
            <span class="Vlt-sidemenu__label">{{ $t(item.meta.label) || getTitle(item.name) }}</span>
          </router-link>
        </li>

        <li role="separator"><h5 class="Vlt-sidemenu__title">{{ $t("message.monitoring") }}</h5></li>
        <li v-for="(item, index) in menu">
          <router-link :to="{ name: item.name, params: { routerRef: router.ref } }"
                       v-if="item.path"
                       class="Vlt-sidemenu__link"
                       active-class="Vlt-sidemenu__link_active" >
            <vlt-icon :icon="item.meta.icon" small class="Vlt-purple" />
            <span class="Vlt-sidemenu__label">{{ $t(item.meta.label) || getTitle(item.name) }}</span>
          </router-link>
        </li>
      </ul>
    </div>
    <div class="Vlt-sidenav__block Vlt-sidenav__block--link Vlt-sidenav__block--border-top ">
      <ul class="Vlt-sidemenu">
        <li>
          <label class="Vlt-sidemenu__link">
            <vlt-icon icon="exit" small />
            <a href="" @click="logout" class="Vlt-sidemenu__label Vlt-logout">{{$t('message.logout')}}</a>
          </label>
        </li>
      </ul>
    </div>
  </nav>
</template>

<script>
  import { mapGetters, mapActions } from 'vuex';
  import { hasClass } from "../../utils/Utils";
  import VltIcon from "../../assets/volta/vue/VltIcon";

  export default {
    components: {
      VltIcon
    },

    props: {
      show: Boolean,
      loggedIn: Boolean,
      name: String,
      role: String,
    },

    data () {
      return {
        isReady: false
      }
    },

    mounted () {
      let route = this.$route;
      if (route.name) {
        this.isReady = true;
        //this.shouldExpandMatchItem(route)
      }
    },

    computed: mapGetters({
      menu: 'menuitems',
      menuGeneral: 'menuitemsGeneral',
      router: 'routerRef',
      sidebar: 'sidebar'

    }),

    methods: {
      ...mapActions([
        'toggleSidebar'
      ]),

      closeMenu(event) {
        if(!hasClass(event.target, 'Vlt-sidenav__scroll') && !hasClass(event.target, 'Vlt-sidemenu__title')) {
          this.toggleSidebar({opened: false});
        }
      },

      logout () {
        this.$emit('logout');
      },

      generatePath (item, subItem) {
        return `${item.component ? item.path + '/' : ''}${subItem.path}`
      },

      getTitle: function(key) {
        return this.$i18n.t(key);
      }
    },

    watch: {
      $route (route) {
        this.isReady = true;
       //this.shouldExpandMatchItem(route)
      },

      'sidebar.opened'(sidebarOpened) {
        if(sidebarOpened) {
          document.addEventListener('click', this.closeMenu);
        } else {
          document.removeEventListener('click', this.closeMenu);
        }
      }
    }

  }
</script>

<style lang="scss">
@import '~assets/volta/scss/lib/_mediaqueries.scss';
@import '~assets/volta/scss/lib/_variables.scss';
@import '~assets/volta/scss/components/_buttons.scss';
@import '~assets/volta/scss/components/_tooltips.scss';
@import '~assets/volta/scss/components/side-navigation/_side-nav--structure.scss';
@import '~assets/volta/scss/components/side-navigation/_side-nav--collapsible.scss';
@import '~assets/volta/scss/components/side-navigation/_side-nav--dark.scss';

.Vlt-sidenav {
  height: 100vh;
}

.Vlt-logout, .Vlt-logout:hover {
  color: white;
}

.Vlt-M-plus {
  @media #{$M-less} {
    display: none;
  }
}
</style>
