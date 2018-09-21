<template>
  <header class="main-header" :class="{ slideInDown: show, slideOutDown: !show }">  
    <a class="Cr-header__logo" href="#" @click="goHome">
      <img class="Cr-header__logo__img" src="~assets/vg-logo.svg" :alt="pkginfo.description">
    </a>
    <div>
      <a class="nav-item is-hidden-tablet" @click="toggleSidebar({opened: !sidebar.opened})">
        <vlt-icon class="Vlt-white" icon="menu" aria-hidden="true" v-if="!sidebar.hidden" />
      </a>
    </div>
    <!--<router-link v-if="!$auth.check()" to="/login" class="nav-item">Login</router-link>-->
    
  </header>
</template>

<script>
  import { mapGetters, mapActions } from 'vuex';
  import { VltIcon } from '../../assets/volta/vue';

  export default {
    components: {
      VltIcon
    },

    props: {
      show: Boolean
    },

    computed: mapGetters({
      pkginfo: 'pkg',
      sidebar: 'sidebar'
    }),

    methods: {
      ...mapActions([
        'toggleSidebar'
      ]),

      goHome: function () {
        this.$router.replace({path: '/'});
      }
    }
  }
</script>

<style lang="scss">
  @import "~assets/style/_styles.scss";
  @import "~assets/volta/scss/lib/_variables.scss";

  .main-header {
    align-items: center;
    background: $blue-dark;
    box-shadow: 1px 0 8px rgba(44, 45, 48, 0.2);
    display: none;
    flex: 0px 0px 100%;
    height: 70px;
    justify-content: space-between;
    transition: height 0.5s;
    padding: 10px 20px;
    width: 100%;
    z-index: 9999999;
  }

  @media screen and (max-width: 768px) {
    .main-header {
      display: flex;
    }
  } 

  .Cr-header__logo {
    align-items: center;
    display: flex;
  }

  .Cr-header__logo__img {
    width: 140px;
  }

  .main-header__logo__text {
    color: $blue-dark;
    font-size: 1.3rem;
    margin-left: 10px;
  }

.app-navbar {
  position: fixed;
  min-width: 100%;
  z-index: 1024;
  box-shadow: 0 2px 3px rgba(17, 17, 17, 0.1), 0 0 0 1px rgba(17, 17, 17, 0.1);
  //display: flex;
  //height: 50px;

  .container {
    margin: auto 10px;
  }

  .nav-right {
    align-items: stretch;
    flex: 1;
    justify-content: flex-end;
    overflow: hidden;
    overflow-x: auto;
    white-space: nowrap;
  }

  nav {
    .nav-left {
      flex-grow: 1;
      padding: 10px;
    }
    .nav-center {
      a {
        display: flex;
        justify-content: center;
        padding-top: 2px;
      }
    }

    img {
      max-height: 25px;
    }

    .logout-link {
      margin-right: 1rem;
      align-self: center;
    }

    display: flex;
    justify-content: space-between;
  }


}

.hero-brand {
  flex-wrap: wrap;
}

</style>
