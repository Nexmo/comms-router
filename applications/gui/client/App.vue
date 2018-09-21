<template>
  <div id="app">
    <nprogress-container></nprogress-container>
    <navbar :show="true"></navbar>
    <sidebar :show="sidebar.opened && !sidebar.hidden" :name="nameUser" :role="roleUser" :logged-in="!!nameUser" @logout="logoutUser"></sidebar>
    <app-main></app-main>
  </div>
</template>

<script>
  import NprogressContainer from 'vue-nprogress/src/NprogressContainer';
  import { Navbar, Sidebar, AppMain } from 'components/layout/';
  import { mapGetters, mapActions } from 'vuex';
  import { isLoggedIn, logout, getUserName, isAdmin, setAuthHandler, showLoginForm } from "./api/auth";
  import { setAuthRequiredHandler } from "./api/api-client";
  import { getRouters } from './api/api-client';

  export default {
    components: {
      Navbar,
      Sidebar,
      AppMain,
      NprogressContainer
    },

    data() {
      return {
        nameUser: null,
        roleUser: null,
      }
    },

    beforeMount() {
      const {body} = document;
      const WIDTH = 768;
      const RATIO = 3;

      const handler = () => {
        if (!document.hidden) {
          let rect = body.getBoundingClientRect();
          let isMobile = rect.width - RATIO < WIDTH;
          this.toggleDevice(isMobile ? 'mobile' : 'other');
        }
      };

      document.addEventListener('visibilitychange', handler);
      window.addEventListener('DOMContentLoaded', handler);
      window.addEventListener('resize', handler);
    },

    mounted() {
      if (isLoggedIn()) {
        this.setLoggedIn();
        this.setDefaultRouter();

      } else {
        logout();
      }
      let self = this;
      setAuthHandler((error) => {
        if (error) {
          self.$dialog.alert({
                title: '',
                message: (typeof error === 'string') ? error : self.$i18n.t('message.loginFailed'),
                confirmText: self.$i18n.t('message.ok').toUpperCase(),
                type: 'is-danger',
                hasIcon: true
              });
        } else if (isLoggedIn()) {
          self.setLoggedIn();
        }
      });
    },

    destroyed() {
      setAuthHandler(null);
      setAuthRequiredHandler(null);
    },

    computed: mapGetters({
      sidebar: 'sidebar'
    }),

    methods: {
      ...mapActions([
        'toggleDevice',
        'setAuthLoggedIn',
        'setRouterRef'
      ]),

      setLoggedIn: function () {
        this.nameUser = getUserName();
        if (!this.nameUser) {
          this.nameUser = this.$i18n.t('message.notSet_');
        }
        this.roleUser = isAdmin() ? this.$i18n.t('message.admin') : this.$i18n.t('message.user');
        this.setAuthLoggedIn(true);

        setAuthRequiredHandler(() => {
          setAuthRequiredHandler(null);
          const self = this;
          this.logoutUser();

          this.$dialog.alert({
            title: '',
            message: this.$i18n.t('message.loginToContinue'),
            type: 'is-danger',
            hasIcon: true,
            icon: 'times-circle',
            iconPack: 'fa'
          })

          // this.$dialog.confirm({
          //   title: '',
          //   message: this.$i18n.t('message.loginToContinue'),
          //   confirmText: this.$i18n.t('message.ok').toUpperCase(),
          //   cancelText: this.$i18n.t("message.cancel"),
          //   type: 'is-warning',
          //   hasIcon: true,
          //   onConfirm: () => {
          //     showLoginForm();
          //   },
          //   onCancel: () => {
          //     self.$router.push({path: '/'});
          //   }
          // });
        });
      },

      setDefaultRouter() {
      	return getRouters()
	        .then((response) => {
	        	const defaultRouter = response.data[0];
            this.setRouterRef(defaultRouter);
	        });
      },

      logoutUser: function () {
        logout();
        this.setAuthLoggedIn(false);
        this.$router.replace({path: '/'});
        this.roleUser = null;
        this.nameUser = null;
      }
    }
  }
</script>

<style lang="scss">
  @import '~animate.css';
  $Vlt-font-url: '~assets/volta/fonts/';
  @import "~assets/volta/scss/volta.scss";

  /*the fade in and out of screens*/
  .animated {
    animation-duration: .377s;
  }

  @import "assets/style/_styles.scss";

  html {
    background-color: whitesmoke;
  }

  #app {
    display: flex;
    flex-flow: row wrap;
  }

  .nprogress-container {
    position: fixed !important;
    width: 100%;
    height: 50px;
    z-index: 2048;
    pointer-events: none;

    #nprogress {
      /*//$color: #48e79a;*/
      $color: $green;

      .bar {
        background: $color;
      }
      .peg {
        box-shadow: 0 0 10px $color, 0 0 5px $color;
      }

      .spinner-icon {
        border-top-color: $color;
        border-left-color: $color;
      }
    }
  }
</style>
