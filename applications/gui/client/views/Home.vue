<template>
  <section>
    <div v-if="isLoggedIn">
      <h1>{{$t('message.welcome')}} {{username}}</h1>
      <h2 class="is-title is-bold">{{ pkg.name.replace(/_/gi, ' ') }}</h2>

      <p>
        <strong>{{ pkg.description }}</strong>
      </p>
    </div>

    <div v-else class="Cr-home">
      <div class="Cr-login">
        <div class="Vlt-card Vlt-card--image">
          <div class="Vlt-card__image Vlt-card__image--blue">
            <img class="Cr-login__logo" src="~assets/vg-logo.svg">
          </div>
          <div class="Vlt-card__content">
            <div class="Vlt-grid">
              <div class="Vlt-col Vlt-col--center">
                <h3>{{ $t('message.loginToCR') }}</h3>
                <button class="Vlt-btn Vlt-btn--large Vlt-btn--primary" @click="login">{{ $t('message.login') }}</button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </section>
</template>

<script>
import { getUserName, showLoginForm } from "../api/auth";
import { mapActions, mapGetters } from 'vuex';
import { getRouters } from '../api/api-client';

export default {
  data () {
    return {
      username: undefined,
      pkg: this.$store.state.pkg
    }
  },

  computed: {
    ...mapGetters({
      isLoggedIn: 'loggedIn'
    })
  },

  methods: {
    ...mapActions([
      'setRouterRef'
    ]),

    login: function () {
      showLoginForm();
    },

    setDefaultRouter() {
      return getRouters()
        .then((response) => {
          const defaultRouter = response.data[0];
          this.setRouterRef(defaultRouter);
        });
    }
  },

  mounted() {
     this.username = getUserName();

     if(!this.$store.state.app.routerRef) {
        this.setDefaultRouter();
     }
  }
}
</script>

<style lang="scss" scoped>
  .Cr-home {
    align-items: center;
    display: flex;
    height: 50vh;
    justify-content: center;
  }

  .Cr-login {
    max-width: 600px;
    width: 100%;
  }

  .Cr-login__logo {
    max-width: 200px;
  }
</style>
